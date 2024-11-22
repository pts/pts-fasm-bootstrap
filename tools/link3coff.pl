#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# link3coff.pl: linker for single COFF objects
# by pts@fazekas.hu at Sun Apr  7 20:43:07 CEST 2024
#
# link3coff.pl is a simple linker which converts a single COFF object file
# to a raw binary image or an ELF-32 Linux i386 executable. It doesn't add
# debug info or symbols. It supports sections .text, .data and .bss (but not
# .rodata or .rdata). The COFF object file can be produced by the Mark
# Williams 80386 assembler (part of Coherent 4.x, see
# https://github.com/pts/pts-mw386as-linux), fixcoff.pl or GNU as(1) from
# Binutils in MinGW (e.g. `i686-w64-mingw32-as --32').
#
# TODO(pts): Add support for .rodata and .rdata when produced by GNU as(1).
#

BEGIN { $^W = 1 }
use integer;
use strict;

my $header_type = "none";  # Default, also `--bin'.
if (@ARGV and $ARGV[0] eq "--elf") { $header_type = "elf"; shift(@ARGV) }  # Shorter ELF header (1 PT_LOAD, 0x54 bytes, like `ld -N').
elsif (@ARGV and $ARGV[0] eq "--elf2") { $header_type = "elf2"; shift(@ARGV) }  # Longer ELF header (2 PT_LOADs, 0x74 bytes, like `ld').
elsif (@ARGV and $ARGV[0] eq "--bin") { $header_type = "none"; shift(@ARGV) }
elsif (@ARGV and $ARGV[0] eq "--") { shift(@ARGV) }
die "Usage: $0 [--bin | --elf | --elf2] <input.o> <output.bin> [<org>]\n" if @ARGV != 3 and @ARGV != 2;
my $org = (!defined($ARGV[2]) ? (($header_type eq "elf" or $header_type eq "elf2") ? 0x8048000 : 0) : $ARGV[2] =~ m@^0[xX]@ ? hex($ARGV[2]) : $ARGV[2] =~ m@^0@ ? oct($ARGV[2]) : int($ARGV[2]));
my $fn = $ARGV[0];
my $fnout = $ARGV[1];

sub fnopenq($) { $_[0] =~ m@[-+.\w]@ ? $_[0] : "./" . $_[0] }
die "fatal: error opening: $fn\n" if !open(F, "< " . fnopenq($fn));
binmode(F);
{ my $oldfd = select(F); $| = 1; select($oldfd); }  # Autoflush.
my $s;
die "fatal: object file to short: $fn\n" if read(F, $s, 0xb4) < 0xb4 - 0x28;
die "fatal: not a COFF object file: $fn\n" if vec($s, 0, 16) != 0x4c01;  # Check in big endian.
my $section_count = unpack("v", substr($s, 2, 2));
#die "fatal: must have 3 or 4 sections: $fn\n" if $section_count != 3 and $section_count != 4;
die "fatal: must have 3 sections: $fn\n" if $section_count != 3;  # .rodata would be next, but mw386as puts it after .bss, and breaks segment-based relocations for .data.
die "fatal: EOF in COFF section definitions: $fn\n" if length($s) < 0x14 + ($section_count * 0x28);
die "fatal: expected .text section" if substr($s, 0x1c - 8, 8) ne ".text\0\0\0";
die "fatal: expected .data section" if substr($s, 0x44 - 8, 8) ne ".data\0\0\0";
die "fatal: expected .bss section" if substr($s, 0x6c - 8, 8) ne ".bss\0\0\0\0";
die "fatal: expected .rodata section" if $section_count >= 4 and substr($s, 0x94 - 8, 8) ne ".rodata\0";

# Even after fixit it, relocations are still wrong.
my($text_vaddr, $text_size) = unpack("VV", substr($s, 0x1c + 4, 8));
die "fatal: paddr--vaddr mismatch in .text: $fn\n" if substr($s, 0x1c, 4) ne substr($s, 0x1c + 4, 4);
my $text_ofs = unpack("V", substr($s, 0x1c + 0xc, 4));
my $text_reloc_ofs = unpack("V", substr($s, 0x1c + 0x10, 4));
my $text_reloc_count = unpack("V", substr($s, 0x1c + 0x18, 4));
my($data_vaddr, $data_size) = unpack("VV", substr($s, 0x44 + 4, 8));
die "fatal: paddr--vaddr mismatch in .data: $fn\n" if substr($s, 0x44, 4) ne substr($s, 0x44 + 4, 4);
my $data_ofs = unpack("V", substr($s, 0x44 + 0xc, 4));
my $data_reloc_ofs = unpack("V", substr($s, 0x44 + 0x10, 4));
my $data_reloc_count = unpack("V", substr($s, 0x44 + 0x18, 4));
my($bss_vaddr, $bss_size) = unpack("VV", substr($s, 0x6c + 4, 8));
die "fatal: paddr--vaddr mismatch in .bss: $fn\n" if substr($s, 0x6c, 4) ne substr($s, 0x6c + 4, 4);
my $is_coff_fixed = ($text_vaddr == 0 and $data_vaddr == 0 and $bss_vaddr == 0);  # Already fixed by fixcoff.pl.
#my $padding_after_text = (-$text_vaddr & 3);

# Load symbols.
my($start_value, $text_value, $data_value, $bss_value);
my($text_sym, $data_sym, $bss_sym);
my $need_sym_load = ($is_coff_fixed or $header_type eq "elf" or $header_type eq "elf2");
if ($need_sym_load) {
  my($sym_ofs, $sym_count) = unpack("VV", substr($s, 8, 8));
  die if !seek(F, $sym_ofs, 0);
  my $ss;
  for (my $i = 0; $i < $sym_count; ++$i) {  # https://web.archive.org/web/20230921074944/https://delorie.com/djgpp/doc/coff/symtab.html
    die if read(F, $ss, 0x12) != 0x12;
    my($name, $value, $scnum, $type, $sclass, $numaux) = unpack("a8VvvCC", $ss);
    $name =~ s@\0+\Z(?!\n)@@;
    # Names longer than 8 bytes are added later. We don't need them.
    #printf STDERR "info: symbol: name=%s value=0x%x scnum=0x%x type=0x%x sclass=0x%x numaux=0x%x\n", $name, $value, $scnum, $type, $sclass, $numaux;
    die if $numaux and read(F, $ss, $numaux * 0x12) != $numaux * 0x12;
    if ($name eq ".text" and $scnum == 1 and $type == 0 and $sclass == 3) { $text_value = $value; $text_sym = $i }
    elsif ($name eq ".data" and $scnum == 2 and $type == 0 and $sclass == 3) { $data_value = $value; $data_sym = $i }
    elsif ($name eq ".bss" and $scnum == 3 and $type == 0 and $sclass == 3) { $bss_value = $value; $bss_sym = $i }
    elsif ($name eq "_start" and $scnum == 1 and $type == 0 and $sclass == 2) { $start_value = $value }
    $i += $numaux;
  }
} else {
  $text_sym = 0; $data_sym = 1; $bss_sym = 2;  # As emitted by mw386as.  !! SVR3 assembler adds some aux entries, so these are incorrect.
}
my %symrs;
$symrs{$text_sym} = 0 if defined($text_sym);
$symrs{$data_sym} = 1 if defined($data_sym);
$symrs{$bss_sym} = 2 if defined($bss_sym);

my @add_vaddrs;
if ($is_coff_fixed) {
  #$text_vaddr = 0;
  #$data_vaddr = $text_size;  # !! Add alignment for .data: 4.
  #$bss_vaddr = $text_size + $data_size;  # !! Add alignment for .bss: 4.
  #push @add_vaddrs, $text_vaddr, $data_vaddr, $bss_vaddr;
  push @add_vaddrs, 0, $text_size, $text_size + $data_size;
  die "fatal: missing some sections of .text, .data, .bss\n" if !defined($text_value) or !defined($data_value) or !defined($bss_value);
  $add_vaddrs[0] += $text_value; $add_vaddrs[1] += $data_value; $add_vaddrs[2] += $bss_value;
} else {
  die "fatal: vaddr--size mismatch in .text\n" if $text_vaddr != 0;
  die "fatal: vaddr--size mismatch in .data\n" if $data_vaddr != $text_size;
  die "fatal: vaddr--size mismatch in .bss\n" if $bss_vaddr != $text_size + $data_size;
  push @add_vaddrs, 0, 0, 0;  # Pre-added by mw386as.
}

my $header = "";
my $do_chmod_x = 0;
if ($header_type eq "elf" or $header_type eq "elf2") {
  # Find `_start' in the symbol table.
  $do_chmod_x = 1;
  $org &= ~0xfff;
  # FYI Modern Linux can't load a program with $org < 0x10000 because of
  # sysctl vm.mmap_min_addr = 65536, but qemu-i386 can.
  die "fatal: missing global symbol, needed for entry point: _start\n" if !defined($start_value);
  my $PT_LOAD = 1;
  my $phdr_count = ($header_type eq "elf2" ? 2 : 1);
  $phdr_count = 1 if $data_size == 0 and $bss_size == 0;
  my $header_size = 0x34 + ($phdr_count << 5);
  my @phdrs;
  if ($phdr_count == 1) {
    my $perm = (($data_size == 0 and $bss_size == 0) ? 5 : 7);
    push @phdrs, pack("V8", $PT_LOAD, 0, $org, $org, $header_size + $text_size + $data_size, $header_size + $text_size + $data_size + $bss_size, $perm, 0x1000);
  } else {
    push @phdrs, pack("V8", $PT_LOAD, 0, $org, $org, $header_size + $text_size, $header_size + $text_size, 5, 0x1000);
    my $vaddr_delta = (($org + $header_size + $text_size) & 0xfff ? 0x1000 : 0);
    push @phdrs, pack("V8", $PT_LOAD, $header_size + $text_size, $org + $header_size + $text_size + $vaddr_delta, $org + $header_size + $text_size + $vaddr_delta, $data_size, $data_size + $bss_size, 6, 0x1000);
    $add_vaddrs[1] += $vaddr_delta; $add_vaddrs[2] += $vaddr_delta;  # Move .data and .bss to the next page.
  }
  die "fatal: assert: bad ELF phdr count\n" if @phdrs != $phdr_count;
  my $OSABI_Linux = 3;
  my $ehdr = pack("Ca3C4x8vvVVVx8v6", 0x7f, "ELF", 1, 1, 1, $OSABI_Linux, 2, 3, 1, $org + $header_size + $start_value, 0x34, 0x34, 0x20, $phdr_count, 0x28, 0, 0);
  $header = join("", $ehdr, @phdrs);
  die "fatal: assert: bad ELF header size\n" if length($header) != $header_size;
  for my $v (@add_vaddrs) { $v += $header_size; }
}

sub add_section($$$$$) {
  my($reloc_ofs, $count, $ofs, $svaddr, $size) = @_;
  die "fatal: relocation count overflow\n" if $count < 0 or $count >= 214748364;  # Should be OK even if multiplied by 10.
  die "fatal: relocation ofs overflow\n" if $reloc_ofs < 0 or ($reloc_ofs >> 31) != 0;
  $count *= 10;
  my $s;
  die if !seek(F, $ofs, 0);
  die if read(F, $s, $size) != $size;
  my $r;
  die if !seek(F, $reloc_ofs, 0);
  die if read(F, $r, $count) != $count;
  for (my $i = 0; $i != $count; $i += 10) {
    my($vaddr, $symndx, $type) = unpack("VVv", substr($r, $i, 10));
    # RELOC_ADDR32=6, RELOC_REL32=0x14==20.
    #printf(STDERR "info: vaddr=0x%x symndx=0x%x type=0x%x\n", $vaddr, $symndx, $type);
    my $delta = 0;
    if ($type == 0x14 and $symndx == $text_sym) {  # IP-relative relocation emitted by the SVR3 assembler for `call' and `jmp'.
      # It is correct to ignore sthis relocation while linking.
      #printf STDERR "debug: delta=0x%x\n", $delta;
    } else {
      die sprintf("fatal: expected reloc type=0x6(RELOC_ADDR32), got 0x%x\n", $type) if $type != 6;
      my $symr = $symrs{$symndx};
      die sprintf("fatal: expected symbol corresponding to section, got 0x%x\n", $symndx) if !defined($symr);
      die sprintf("fatal: expected vaddr at least 0x%x, got 0x%x\n", $svaddr, $vaddr) if $svaddr > $vaddr;
      die sprintf("fatal: vaddr too large: 0x%x\n", $vaddr) if $vaddr - $svaddr + 4 > length($s);
      $delta = $org + $add_vaddrs[$symr];
    }
    next if !$delta;
    #printf(STDERR "info: fix vaddr=0x%x delta=0x%x d=0x%x\n", $vaddr - $svaddr, $delta, unpack("V", substr($s, $vaddr - $svaddr, 4)));
    substr($s, $vaddr - $svaddr, 4) = pack("V", unpack("V", substr($s, $vaddr - $svaddr, 4)) + $delta);
  }
  die if !print(FOUT $s);
}

unlink($fnout);  # To avoid the `Text file busy' error.
die "fatal: error opening: $fnout\n" if !open(FOUT, ">" . fnopenq($fnout));
binmode(FOUT);
{ my $oldfd = select(FOUT); $| = 1; select($oldfd); }  # Autoflush.
die if !print(FOUT $header);
add_section($text_reloc_ofs, $text_reloc_count, $text_ofs, $text_vaddr, $text_size);
add_section($data_reloc_ofs, $data_reloc_count, $data_ofs, $data_vaddr, $data_size);
die "fatal: error closing: $fnout\n" if !close(FOUT);
if ($do_chmod_x) {  # Make it executable.
  my @stat = stat($fnout);
  if (@stat) {
    my $mode = ($stat[2] & 01777);
    my $mode2 = ($mode | ($mode & 4 ? 1 : 0) | ($mode & 040 ? 010 : 0) | ($mode & 0400 ? 0100 : 0));
    if ($mode != $mode2) {
      chmod($mode2, $fnout);  # Fail silently, e.g. on Windows.
    }
  }
}
#print "info: linking OK, output file written: $fnout\n";

__END__

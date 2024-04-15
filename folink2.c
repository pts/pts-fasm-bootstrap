/*
 * folink2.c: flat OMF linker implemented in C with read(...) and write(...)
 * by pts@fazekas.hu at Tue Mar 26 02:24:06 CET 2024
 *
 * This linker just dumps the data in the LEDATA, LEDATA386, LIDATA and
 * LIDATA386 records in the input OMF file. For the LIDATA and LIDATA386
 * records, it expands the repeated bytes. It fails if it encounters
 * multiple segments or any relocations (fixupps).
 */

#define WRITE_STDERR(strlit) (void)!write(2, strlit CRLF, sizeof(strlit CRLF) - 1)
#ifdef __DOSMC__  /* Size-optimized DOS 8086 target. https://github.com/pts/dosmc */
#  include <dosmc.h>
#  define CHECK(exit_code, cond, msg) if (!(cond)) fatal(exit_code)
#  undef  DEBUG
#  define DEBUG 0
#  define CRLF "\r\n"
#  define open(pathname, flags, mode) creat(pathname, 0)  /* Used for creat(...). */
#  undef  O_BINARY
  void fatal(int exit_code) {
    WRITE_STDERR("folink2 fatal error");
    exit(exit_code);
  }
#else
#  ifndef DEBUG
#    define DEBUG 0
#  endif
#  if DEBUG
#    include <stdio.h>  /* fprintf() for debugging. */
#  endif
#  include <stdlib.h>  /* exit(). */
#  include <fcntl.h>  /* open(), O_BINARY. */
#  if defined(MSDOS) || defined(_WIN32)
#    include <io.h>  /* open(), setmode(). */
#  else
#    include <unistd.h>  /* read(), write(). */
#    undef O_BINARY
#  endif
#  define CRLF "\n"
#  define open2 open
#  if DEBUG
#    define CHECK(exit_code, cond, msg) if (!(cond)) fatal("check: " msg, exit_code)
    void fatal(const char *msg, int exit_code) {
      fprintf(stderr, "fatal: %s (%d)\n", msg, exit_code);
      exit(exit_code);
    }
#  else
#    define CHECK(exit_code, cond, msg) if (!(cond)) fatal(exit_code)
    void fatal(int exit_code) {
      WRITE_STDERR("folink2 fatal error\n");
      exit(exit_code);
    }
#  endif
#endif
#ifndef O_BINARY
#  define O_BINARY 0
#endif

#ifndef uu
typedef unsigned short uu;  /* At least 16 bits, unsigned. */
#endif

#ifndef FOLINK_BUFSIZE
#  if defined(MSDOS)
#    define FOLINK_BUFSIZE 0x200
#  else
#    define FOLINK_BUFSIZE 0x2000
#  endif
#endif

char rdbuf[FOLINK_BUFSIZE];
uu rdi, rdlimit;
int rdfd;
uu rsize;  /* Record size. Number of bytes remaining from the current OMF record. */
uu r8(void) {  /* Reads a byte from the input OMF file. */
  int i;
  CHECK(3, rsize != 0, "bad record content size");
  if (rdi == rdlimit) {
    i = read(rdfd, rdbuf, sizeof(rdbuf));
    CHECK(5, i > 0, "unexpected EOF within record");
    rdlimit = i;
    rdi = 0;
  }
  --rsize;
  return (unsigned char)rdbuf[rdi++];
}

char wrbuf[FOLINK_BUFSIZE];
uu wri;
int wrfd;
void wflush(void) {
  int i;
  if (wri > 0) {
    i = write(wrfd, wrbuf, wri);
    CHECK(7, i > 0 && (uu)i == wri, "write error");
    wri = 0;
  }
}
void w8(char c) {  /* Writes a byte to the output raw binary file. */
  if (wri == sizeof(wrbuf)) wflush();
  wrbuf[wri++] = c;
}

uu r16_le(void) {  /* Reads 2 bytes in little-endian order from the input OMF file. */
  uu result = r8();
  return result | r8() << 8;
}

/* Supported OMF record types. */
#define COMENT 0x88
#define GRPDEF 0x9a
#define LEDATA 0xa0
#define LIDATA 0xa2
#define LEDATA386 0xa1
#define LIDATA386 0xa3
#define LNAMES 0x96
#define MODEND 0x8a
#define MODEND386 0x8b
#define SEGDEF 0x98
#define SEGDEF386 0x99
#define THEADR 0x80

int main(int argc, char **argv) {
  uu rtype, dofs, ofs;
  uu bc0, bc1, bs, rv;
  unsigned long rc0, ri0, rc1, ri1;  /* At least 32 bits. */
  uu seg_idx;
  char is_ofs_ok;
  (void)argc;
  if (!argv[1] || !argv[2] || argv[3]) {
    WRITE_STDERR("Usage: folink2 <in.obj> <out.bin>");
    return 1;
  }
  if ((rdfd = open2(argv[1], O_RDONLY | O_BINARY)) < 0) CHECK(20, 0, "error opening input file");
  if ((wrfd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0666)) < 0) CHECK(21, 0, "error opening output file");
  ofs = 0; is_ofs_ok = 0;
  for (;;) {
    rsize |= 3;  /* Don't trigger the record content size check in r16_le() below. Any value at least 3 will do. */
    rtype = r8();
#if DEBUG
    fprintf(stderr, "info: rtype=0x%02x\n", rtype);
#endif
    rsize = r16_le();
    CHECK(2, rsize > 0, "rsize must be positive");
    --rsize;  /* Don't count the checksum byte now. */
    if (rtype == MODEND || rtype == MODEND386) {
      break;
    } else if (rtype == COMENT || rtype == GRPDEF || rtype == LNAMES || rtype == SEGDEF || rtype == SEGDEF386 || rtype == THEADR) {
      while (rsize > 0) {  /* Skip and ignore. */
        r8();
      }
    } else if (rtype == LEDATA || rtype == LIDATA || rtype == LEDATA386 || rtype == LIDATA386) {
      seg_idx = r8();  /* Segment index. */
      CHECK(11, seg_idx == 1, "unexpected LEDATA/LIDATA segment");
      dofs = r16_le();
      if (rtype == LEDATA386 || rtype == LIDATA386) r16_le();  /* Skip high word. */
      if (is_ofs_ok) {
        CHECK(4, dofs == ofs, "bad ofs");
      } else {
        ofs = dofs;
        ++is_ofs_ok;  /* Set it to true. */
      }
      if (rtype == LEDATA || rtype == LEDATA386) {
        ofs += rsize;
        while (rsize > 0) {
          w8(r8());
        }
      } else {  /* LIDATA or LIDATA386. */
        rc0 = r16_le();
        if (rtype == LIDATA386) rc0 |= (unsigned long)r16_le() << 16;
        bc0 = r16_le();
        CHECK(6, bc0 != 0, "unsupported LIDATA direct block0");  /* Not in TASM output. */
        for (; bc0 > 0; --bc0) {
          rc1 = r16_le();
          if (rtype == LIDATA386) rc1 |= (unsigned long)r16_le() << 16;
          bc1 = r16_le();
          CHECK(8, bc1 == 0, "LIDATA block1 block count must be 0");
          bs = r8();
          rv = r8();
          CHECK(9, bs == 1, "LIDATA block1 block size must be 1");
          for (ri0 = rc0; ri0 > 0; --ri0) {
            ofs += rc1;
            for (ri1 = rc1; ri1 > 0; --ri1) {
              w8(rv);
            }
          }
        }
      }
    } else {
      CHECK(18, 0, "unsupported rtype");
    }
    ++rsize;
    r8();  /* Skip checksum byte. */
    CHECK(10, rsize == 0, "record too long");
  }
  wflush();  /* Just for error checking. */
  return 0;
}

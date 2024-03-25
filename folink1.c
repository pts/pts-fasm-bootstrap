/*
 * folink1.c: flat OMF linker implemented in C with read(...) and write(...)
 * by pts@fazekas.hu at Mon Mar 25 00:49:38 CET 2024
 */

#ifdef __DOSMC__
#  include <dosmc.h>
#  define CHECK(exit_code, cond, msg) if (!(cond)) fatal(exit_code)
#  undef  DEBUG
#  define DEBUG 0
  void fatal(int exit_code) {
    write(2, "folink1 fatal error\r\n", 21);
    exit(exit_code);
  }
#else
#  ifndef DEBUG
#    define DEBUG 0
#  endif
#  include <stdio.h>  /* fprintf() for debugging. */
#  include <stdlib.h>  /* exit(). */
#  include <unistd.h>  /* read(), write(). */
#  define CHECK(exit_code, cond, msg) if (!(cond)) fatal("check: " msg, __LINE__, exit_code)
  void fatal(const char *msg, int line, int exit_code) {
    fprintf(stderr, "fatal: %s (%d)\n", msg, line);
    exit(exit_code);
  }
#endif

typedef unsigned short uu;  /* At least 16 bits, unsigned. */

char rdbuf[0x200];
uu rdi, rdlimit;
int rdfd;
uu rsize;  /* Record size. Number of bytes remaining from the current OMF record. */
uu r8(void) {  /* Reads a byte from the input OMF file. */
  int i;
  CHECK(3, rsize != 0, "bad record content size");
  if (rdi == rdlimit) {
    i = read(rdfd, rdbuf, sizeof(rdbuf));
    CHECK(5, i >= 0, "unexpected EOF within record");
    rdlimit = i;
    rdi = 0;
  }
  --rsize;
  return (unsigned char)rdbuf[rdi++];
}

char wrbuf[0x200];
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

char ledata[0x400];  /* Data bytes read from the last LEDATA record, possibly modified by subsequent FIXUPP386 records. */
uu ledata_size;  /* Number of bytes populated at the beginning of ledata. */

void flush_ledata(void) {  /* Writes (flushes) all the bytes from the ledata array to w8(...). */
  char *p = ledata, *q = p + ledata_size;
  for (; p != q; ++p) {
    w8(*p);  /* TODO(pts): Flush with fewer function calls. */
  }
  ledata_size = 0;
}

#ifdef __DOSMC__
  static __inline void add_le32(char *p, unsigned long v) {  /* Adds the value v to the little-endian 32-bit number at p. */
    *(unsigned long*)p += v;
  }
#else
  void set_le32(char *p, unsigned long v) {
    *p++ = v; v >>= 8;
    *p++ = v; v >>= 8;
    *p++ = v; v >>= 8;
    *p = v;
  }
  unsigned long get_le32(const char *p) {
    const unsigned char *pu = (const unsigned char*)p;
    return ((unsigned)pu[0] | (unsigned)pu[1] << 8) | (unsigned long)((unsigned)pu[2] | (unsigned)pu[3] << 8) << 16;
  }
  void add_le32(char *p, unsigned long v) {  /* Adds the value v to the little-endian 32-bit number at p. */
    set_le32(p, get_le32(p) + v);
  }
#endif

#define ORG 0x8048000UL  /* !! Make it configurable, or read it from the ELF header. */

/* Supported OMF record types. */
#define COMENT 0x88
#define FIXUPP386 0x9d
#define GRPDEF 0x9a
#define LEDATA 0xa0
#define LIDATA 0xa2
#define LNAMES 0x96
#define MODEND 0x8a
#define SEGDEF 0x98
#define THEADR 0x80

int main(int argc, char **argv) {
  uu rtype, dofs, ofs;
  uu rc0, ri0, bc0, rc1, ri1, bc1, bs, rv;  /* LIDATA repeat_count, block_count, block_size, repeat_value. */
  uu fix_byte, fix_ofs;  /* FIXUPP subrecord fields. */
  char *ledata_p;
  (void)argc; (void)argv;
  ofs = 0; ledata_size = 0;
  wrfd = 1;
  for (;;) {
    rsize |= 3;  /* Don't trigger the record content size check in r16_le() below. */
    rtype = r8();
#if DEBUG
    fprintf(stderr, "info: rtype=0x%02x\n", rtype);
#endif
    rsize = r16_le();
    CHECK(2, rsize > 0, "rsize must be positive");
    --rsize;  /* Don't count the checksum byte now. */
    if (rtype == MODEND) {
      break;
    } else if (rtype == COMENT || rtype == GRPDEF || rtype == LNAMES || rtype == SEGDEF || rtype == THEADR) {
      while (rsize > 0) {  /* Skip and ignore. */
        r8();
      }
    } else if (rtype == LEDATA || rtype == LIDATA) {
      flush_ledata();
      CHECK(19, rsize <= sizeof(ledata), "LEDATA/LIDATA data too large");  /* TASM output limit. */
      fix_byte = r8();  /* Segment index. */
      CHECK(11, fix_byte == 1, "unexpected LEDATA/LIDATA segment");
      dofs = r16_le();
      CHECK(4, dofs == ofs, "bad ofs");
      if (rtype == LEDATA) {
        ledata_size = rsize;
        ofs += rsize;
        for (ledata_p = ledata; rsize > 0; ++ledata_p) {
          *ledata_p = r8();  /* Buffer it for a subsequent FIXUPP. */
        }
      } else {  /* LIDATA. */
        rc0 = r16_le();
        bc0 = r16_le();
        CHECK(6, bc0 != 0, "unsupported LIDATA direct block0");  /* Not in TASM output. */
        for (; bc0 > 0; --bc0) {
          rc1 = r16_le();
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
    } else if (rtype == FIXUPP386) {
      while (rsize > 0) {
        fix_byte = r8();
        CHECK(12, (fix_byte & 0x80) != 0, "FIXUPP thread subrecord found");
        fix_ofs = r8() | (fix_byte & 3) << 8;
        CHECK(13, fix_byte >> 2 == 0x39, "unexpected FIXUPP is_segrel or location");
        fix_byte = r8();
        CHECK(14, fix_byte == 0x14, "unexpected FIXUPP target");
        fix_byte = r8();
        CHECK(15, fix_byte == 1, "unexpected FIXUPP td");
        fix_byte = r8();
        CHECK(16, fix_byte == 1, "unexpected FIXUPP fd");
        CHECK(17, fix_ofs + 4 <= ledata_size, "FIXUP ofs too large");
        add_le32(ledata + fix_ofs, ORG);  /* Process relocation. */
      }
    } else {
      CHECK(18, 0, "unsupported rtype");
    }
    ++rsize;
    r8();  /* Skip checksum byte. */
    CHECK(10, rsize == 0, "record too long");
  }
  flush_ledata();
  wflush();
#ifdef __DOSMC__
  exit(0);
#endif
  return 0;
}

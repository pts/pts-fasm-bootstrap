/*
 * folink2c.js: flat OMF linker implemented in Windows Scripting Host JScript
 * by pts@fazekas.hu at Tue Apr 16 00:06:24 CEST 2024
 *
 * This linker just dumps the data in the LEDATA, LEDATA386, LIDATA and
 * LIDATA386 records in the input OMF file. For the LIDATA and LIDATA386
 * records, it expands the repeated bytes. It fails if it encounters
 * multiple segments or any relocations (fixupps).
 *
 * This linker keeps both the input and the output files in memory, so it is
 * not suitable for long files.
 *
 * Usage on Windows: cscript /nologo folink2c.js IN.OBJ OUT.BIN
 * Usage on Windows NT using folink2c.cmd: folink2c IN.OBJ OUT.BIN
 */

var fso = WSH.CreateObject("Scripting.FileSystemObject");
var stderr = fso.GetStandardStream(2);  /* Good enough for ASCII. */

if (WSH.Arguments.Length != 2) {
  stderr.WriteLine("Usage: folink2 <in.obj> <out.bin>");
  WSH.Quit(1);
}

function check(exit_code, condition, message) {
  if (condition) return;
  stderr.WriteLine("fatal: " + message + " (" + exit_code + ")");
  WSH.Quit(exit_code);
}

var TypeBinary = 1, TypeText = 2, ForReading = 1, ForWriting = 2;

/* ADODB.Stream is disabled on some Windows configurations (such as Windows
 * 2004 server), but the alternative, Scripting.FileSystemObject cannot read
 * or write binary files: it does byte-to-character transformation based on
 * the system locale, and it fails for charsets with multibyte characters.
 *
 * More info: https://stackoverflow.com/questions/6060529/read-and-write-binary-file-in-vbscript
 * More info: https://stackoverflow.com/a/64736654/97248
 */
var fin = WSH.CreateObject("ADODB.Stream");
fin.Open();
fin.Type = TypeText;
fin.Charset = "iso-8859-2";  /* Not using "iso-8859-1", because Windows seems to be doing something strange (Windows-1250?) on top of that, but only in rtrans. */
var rtrans = {
    260: 161, 728: 162, 321: 163, 317: 165, 346: 166, 352: 169, 350: 170, 356:
    171, 377: 172, 381: 174, 379: 175, 261: 177, 731: 178, 322: 179, 318: 181,
    347: 182, 711: 183, 353: 185, 351: 186, 357: 187, 378: 188, 733: 189, 382:
    190, 380: 191, 340: 192, 258: 195, 313: 197, 262: 198, 268: 200, 280: 202,
    282: 204, 270: 207, 272: 208, 323: 209, 327: 210, 336: 213, 344: 216, 366:
    217, 368: 219, 354: 222, 341: 224, 259: 227, 314: 229, 263: 230, 269: 232,
    281: 234, 283: 236, 271: 239, 273: 240, 324: 241, 328: 242, 337: 245, 345:
    248, 367: 249, 369: 251, 355: 254, 729: 255};

var fout = WSH.CreateObject("ADODB.Stream");
fout.Open();
fout.Type = TypeText;
fout.Charset = "iso-8859-2";
var wtrans = {
    161: 260, 162: 728, 163: 321, 165: 317, 166: 346, 169: 352, 170: 350, 171:
    356, 172: 377, 174: 381, 175: 379, 177: 261, 178: 731, 179: 322, 181: 318,
    182: 347, 183: 711, 185: 353, 186: 351, 187: 357, 188: 378, 189: 733, 190:
    382, 191: 380, 192: 340, 195: 258, 197: 313, 198: 262, 200: 268, 202: 280,
    204: 282, 207: 270, 208: 272, 209: 323, 210: 327, 213: 336, 216: 344, 217:
    366, 219: 368, 222: 354, 224: 341, 227: 259, 229: 314, 230: 263, 232: 269,
    234: 281, 236: 283, 239: 271, 240: 273, 241: 324, 242: 328, 245: 337, 248:
    345, 249: 367, 251: 369, 254: 355, 255: 729};

try { fin.LoadFromFile(WSH.Arguments(0)); }
catch (exc) { check(20, 0, "error opening input file"); }

var rsize = 0;
function r8() {  /* Reads a byte from the input OMF file. */
  check(3, rsize != 0, "bad record content size");
  --rsize;
  var s = fin.ReadText(1);
  check(5, s.length == 1, "unexpected EOF within record");
  s = s.charCodeAt(0);
  return rtrans[s] || s;
}

function rskip() {  /* Reads and skips rsize bytes. */
  if (rsize > 0) {
    var got = fin.ReadText(rsize).length;
    check(22, got == rsize, "unexpected EOF when skipping");
  }
  rsize = 0;
}

function r16_le() {  /* Reads 2 bytes in little-endian order from the input OMF file. */
  var result = r8();
  return result | r8() << 8;
}

function w8(i) {
  i &= 0xff;
  fout.WriteText(String.fromCharCode(wtrans[i] || i));
}

function rw() {  /* Copy rsize bytes from fin to fout. */
  if (rsize > 0) {
    var s = fin.ReadText(rsize);
    check(23, s.length == rsize, "unexpected EOF when copying");
    fout.WriteText(s);  /* This is fast, because it skips rtrans + wtrans. */
  }
  rsize = 0;
}  

/* Supported OMF record types. */
var COMENT = 0x88, GRPDEF = 0x9a, LEDATA = 0xa0, LIDATA = 0xa2;
var LEDATA386 = 0xa1, LIDATA386 = 0xa3, LNAMES = 0x96, MODEND = 0x8a;
var MODEND386 = 0x8b, SEGDEF = 0x98, SEGDEF386 = 0x99, THEADR = 0x80;

var ofs = 0, is_ofs_ok = false;
var seg_idx, dofs, rtype, bc0, bc1, bs, rv, rc0;
var ri0, rc1, ri1;  /* At least 32 bits. TODO(pts): Make it work for negative. */
for (;;) {  /* No need to check fin.AtEndOfStream. */
    rsize |= 3;  /* Don't trigger the record content size check in r16_le() below. Any value at least 3 will do. */
    rtype = r8();
    rsize = r16_le();
    check(2, rsize > 0, "rsize must be positive");
    --rsize;  /* Don't count the checksum byte now. */
    if (rtype == MODEND || rtype == MODEND386) {
      break;
    } else if (rtype == COMENT || rtype == GRPDEF || rtype == LNAMES || rtype == SEGDEF || rtype == SEGDEF386 || rtype == THEADR) {
      rskip();
    } else if (rtype == LEDATA || rtype == LIDATA || rtype == LEDATA386 || rtype == LIDATA386) {
      seg_idx = r8();  /* Segment index. */
      check(11, seg_idx == 1, "unexpected LEDATA/LIDATA segment");
      dofs = r16_le();
      if (rtype == LEDATA386 || rtype == LIDATA386) r16_le();  /* Skip high word. */
      if (is_ofs_ok) {
        check(4, dofs == ofs, "bad ofs");
      } else {
        ofs = dofs;
        is_ofs_ok = true;
      }
      if (rtype == LEDATA || rtype == LEDATA386) {
        ofs += rsize; ofs &= 0xffff;
        rw();
      } else {  /* LIDATA or LIDATA386. */
        rc0 = r16_le();
        if (rtype == LIDATA386) rc0 |= r16_le() << 16;
        bc0 = r16_le();
        check(6, bc0 != 0, "unsupported LIDATA direct block0");  /* Not in TASM output. */
        for (; bc0 > 0; --bc0) {
          rc1 = r16_le();
          if (rtype == LIDATA386) rc1 |= r16_le() << 16;
          bc1 = r16_le();
          check(8, bc1 == 0, "LIDATA block1 block count must be 0");
          bs = r8();
          rv = r8();
          check(9, bs == 1, "LIDATA block1 block size must be 1");
          for (ri0 = rc0; ri0 != 0; --ri0, ri0 |= 0) {  /* `|= 0' to do unsigned 32-bit wraparound. */
            ofs += rc1; ofs &= 0xffff;
            for (ri1 = rc1; ri1 != 0; --ri1, ri1 |= 0) {
              w8(rv);
            }
          }
        }
      }
    } else {
      check(18, 0, "unsupported rtype");
    }
    ++rsize;
    r8();  /* Skip checksum byte. */
    check(10, rsize == 0, "record too long");
}

try { fout.SaveToFile(WSH.Arguments(1), ForWriting); }
catch (exc) { check(21, 0, "error opening output file"); }

/* __END__ */

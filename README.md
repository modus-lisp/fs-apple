# fs-apple

Apple disk and file formats in pure Common Lisp: no FFI, no mounting. A disk or
file is a byte vector, read and written in place. The package is `FS-APPLE`.

It is written to run wherever Common Lisp does: SBCL, and Modus on x86-64, on the
68k under qemu-user, and on a Macintosh (a 30-bit-fixnum Lisp, so no 32-bit field
is assumed to fit a fixnum).

## What is here

| Format | File | Functions |
|---|---|---|
| Apple Partition Map (driver descriptor + map) | `lib/apm.lisp` | `apm-read`, `apm-write-ddm`, `apm-write-map`, `apm-boot-checksum` |
| PackBits (TN1023) | `lib/packbits.lisp` | `packbits`, `unpackbits` |
| A file's resource fork (Inside Macintosh: Resource Manager) | `lib/resource-fork.lisp` | `resource-fork-write`, `resource-fork-read` |
| MacBinary II | `lib/macbinary.lisp` | `macbinary-write`, `macbinary-read` (the header's CRC-16 is cram's `crc16`) |
| AppleDouble (RFC 1740) | `lib/appledouble.lisp` | `appledouble-read`, `appledouble-write` |
| Disk Copy 4.2 and 6 (NDIF) images | `lib/diskcopy.lisp` | `diskcopy42-read`, `ndif-read` (NDIF's ADC is cram's `adc-decompress`) |
| HFS volumes | `lib/hfs.lisp` | `hfs-read`, `hfs-write`; a volume is an `hfs-folder` (`hfs-ref`, `hfs-set`, `hfs-remove`, `hfs-paths`), a file an `hfs-file` |
| Mac OS Roman | `lib/macroman.lisp` | `mac-roman-decode`, `mac-roman-encode` (HFS and resource names) |

`hfs-write` is machfs's writer, decision for decision, so the same tree makes the same
volume: System 6's boot blocks from the System file, the startup application, aliases
rebuilt for the volume.  Planned: BinHex.

MacBinary needs [cram](https://github.com/modus-lisp/cram) (`fs-apple.asd` depends on it).

```lisp
(asdf:load-system "fs-apple")
(multiple-value-bind (ddm entries) (fs-apple:apm-read disk-bytes)
  (getf ddm :block-count)            ; blocks on the disk
  (mapcar (lambda (e) (getf e :type)) entries))  ; "Apple_partition_map" ...
```

## Tests

`test/apm-test.lisp` is a plain CL script: `sbcl --script test/apm-test.lisp`,
or the same file under Modus. Its last line is `APM-TEST PASS` or `APM-TEST FAIL`.

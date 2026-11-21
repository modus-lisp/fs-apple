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

Planned, each its own file: HFS volumes, AppleDouble, BinHex.

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

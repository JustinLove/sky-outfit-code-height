// lz4.js - An implementation of Lz4 in plain JavaScript.
// Function extracted from https://github.com/Benzinga/lz4js

// Compression format parameters/constants.
var minMatch = 4;

var lz4 = {}

// Decompresses a block of Lz4.
lz4.decompressBlock = function decompressBlock (src, dst, sIndex, sLength, dIndex) {
  var mLength, mOffset, sEnd, n, i;
  var hasCopyWithin = dst.copyWithin !== undefined && dst.fill !== undefined;

  // Setup initial state.
  sEnd = sIndex + sLength;

  // Consume entire input block.
  while (sIndex < sEnd) {
    var token = src[sIndex++];

    // Copy literals.
    var literalCount = (token >> 4);
    if (literalCount > 0) {
      // Parse length.
      if (literalCount === 0xf) {
        while (true) {
          literalCount += src[sIndex];
          if (src[sIndex++] !== 0xff) {
            break;
          }
        }
      }

      // Copy literals
      for (n = sIndex + literalCount; sIndex < n;) {
        dst[dIndex++] = src[sIndex++];
      }
    }

    if (sIndex >= sEnd) {
      break;
    }

    // Copy match.
    mLength = (token & 0xf);

    // Parse offset.
    mOffset = src[sIndex++] | (src[sIndex++] << 8);

    // Parse length.
    if (mLength === 0xf) {
      while (true) {
        mLength += src[sIndex];
        if (src[sIndex++] !== 0xff) {
          break;
        }
      }
    }

    mLength += minMatch;

    // Copy match
    // prefer to use typedarray.copyWithin for larger matches
    // NOTE: copyWithin doesn't work as required by LZ4 for overlapping sequences
    // e.g. mOffset=1, mLength=30 (repeach char 30 times)
    // we special case the repeat char w/ array.fill
    if (hasCopyWithin && mOffset === 1) {
      dst.fill(dst[dIndex - 1] | 0, dIndex, dIndex + mLength);
      dIndex += mLength;
    } else if (hasCopyWithin && mOffset > mLength && mLength > 31) {
      dst.copyWithin(dIndex, dIndex - mOffset, dIndex - mOffset + mLength);
      dIndex += mLength;
    } else {
      for (i = dIndex - mOffset, n = i + mLength; i < n;) {
        dst[dIndex++] = dst[i++] | 0;
      }
    }
  }

  return dIndex;
};

# Sky Outfit Code Height

The Outfit QR Codes in [Sky: Children of the Light](https://www.thatskygame.com/) contain a visual description of the game character, including the exact height and scale values. This is a small clientside-only web app that decodes the character data to display the height values.

Running at [https://wondible.com/sky-outfit-code-height/](https://wondible.com/sky-outfit-code-height/)

## Technical Information

A JSON document containing the character description is [LZ4 block compressed](https://github.com/lz4/lz4/blob/dev/doc/lz4_Block_format.md) and base64-uri encoded. The base64 string is included in a url format, which is presented in a QR-code. (The QR codes are very dense and can be difficult to read, using a high resolution display may be helpful.)

The first version of the code observed used full property names and outfit objects. The second version used short property names and arrays of values. Unfortuately, the 'h' key is duplicated in this version so a simple json decode will not recover both properties.

## Building

Built using [Elm](http://elm-lang.org/)

My build command:

> elm make src/SkyOutfitCodeHeight.elm --output public/sky-outfit-code-height.js

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be served for a local or internet server as you wish. `elm reactor` provides a basic local web server.

## Credits

[QR Scanner](https://github.com/nimiq/qr-scanner)
[Lz4.js](https://github.com/Benzinga/lz4js) - extracted decompressBlock function

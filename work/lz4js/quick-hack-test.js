base = '8RZ7ImJvZHkiOnsiaWQiOjE5OTYzODIwMjYsInRleCI6MCwicGF0CABBbWFzawkAwGR5ZSI6Iihub25lLAUAlCkifSwid2luZ0YAnzI2Njg5ODM0MUUAHUVoYWlyRQCfOTIyODcwODA0RgAdAh4AA0YAjzU5MjUwMTQz0QAeNW5lY0YAnzM4MDA4ODQ2OdIAHkVmZWV0XQGfNTMyMDkzOTkwGAEeNW9ybowAnzY4MDQ5OTIyOYwAHjVhY2VGAJ81OTk5ODY4ODVGAB1FcHJvcEYAjzQxOTI3NDI40gAfFWEXAZ80MTc3OTQzNTajAR7wG2hlaWdodCI6MS40ODgyNTM4LCJzY2FsZSI6LTAuMDM1ODE0NTUsInZvafkAsTEsImF0dGl0dWRlUwAxc2VlIQLwCDIwMiwicmVmcmVzaHZlcnNpb24iOjB9'
buffer = Buffer.from(base, 'base64')
output = new Array(2000)
lz4 = require('./lz4')
r = lz4.decompressBlock(buffer, output, 0, buffer.length, 0)
trunc = output.slice(0,r)
s = new TextDecoder().decode(Uint8Array.from(trunc))
console.log(s)

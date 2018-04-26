NB. supports bitmap files
NB.
NB. These are .bmp files in DIB format.
NB.
NB. readbmp and writebmp use RGB values (single numbers).
NB.
NB. writebmp also accepts RGB triples
NB.
NB. main functions:
NB.
NB.   readbmp      read bitmap file, returning RGB data
NB.   writebmp     write bitmap file from RGB data
NB.   readbmphdr   read header from bitmap file

NB. =========================================================
NB.*readbmp v read bitmap file, returning RGB data
readbmp=: 3 : 0

r=. readbmphdrall y
if. 2 = 3!:0 r do. return. end.
'nos dat'=. r
'bits rws cls off shdr'=. nos

pal=. off {. dat
dat=. off }. dat

if. bits e. 1 4 8 do.
  pal=. 256 #. flipreadrgb"1 a. i. _4 }: \ (shdr+14) }. pal
  dat=. , ((#~ ^.&256) 2^bits) #: a. i. dat
  pal {~ |. (rws,cls){.(rws,cls+(32%bits)|-cls) $ dat
elseif. bits=24 do.
  cl4=. 4 * >. (3*cls) % 4
  |. (rws,cls) {. 256 #. flipreadrgb"1 a.i. _3 [\"1 (rws,cl4) $ dat
elseif. 1 do.
  'only 1,4,8 and 24-bit bitmaps supported, this is ',(":bits),'-bit'
end.
)

NB. =========================================================
NB.*readbmphdr v read header from bitmap file
NB. returns:  bitsize, rows, columns
readbmphdr=: 3 : 0
r=. readbmphdrall y
if. 2 ~: 3!:0 r do.
  3 {. 0 pick r
end.
)

NB. =========================================================
NB.*readbmpall v read bitmap data
NB.
NB. y is bitmap file, or bitmap data
NB. returns:  bitsize, rows, columns, offset, sheader, data
readbmphdrall=: 3 : 0

try.
  dat=. 1!:1 boxopen y
  if. -. 'BM'-:2{.dat do. 'not a bitmap file' return. end.
catch.
  dat=. y
  if. -. 'BM'-:2{.dat do. 'file read error' return. end.
end.

toi=. 256&#.@(a.&i.)@(|."1)
bits=. toi 28 29 {dat

if. toi 30 31 32 33{dat do.
  'compressed format not supported' return.
end.

'off shdr cls rws'=. toi (10+i.4 4){dat
(bits,rws,cls,off,shdr);dat
)

NB. =========================================================
NB.*writebmp v write bmp file from RGB data
NB.
NB. Form:  data writebmp filename [;minimum bitsize]
NB.
NB. picks appropriate bit size of 4 8 or 24, subject
NB. to optional minimum bit size.

writebmp=: 4 : 0

dat=. x
'file min'=. 2 {. (boxopen y), <0

if3=. (3=#$dat) *. 3={:$dat

sbmp=. (-if3) }. $dat
pal=. /:~ ~. ,/ dat
bit=. 4 8 24 {~ +/ 16 256 < # pal
bit=. min >. bit

if. bit e. 4 8 do.
  dat=. pal i. dat
  if. bit=4 do.
    dat=. 16 #. _2[\"1 dat
  else.
    pal=. 256 {. pal
  end.
  spal=. #pal
  if. -. if3 do.
    pal=. 256 256 256 #: pal
  end.
  pal=. flipwritergb"1 pal
  pal=. , a. {~ pal ,. 0
  xsbmp=. ($dat) + 0 1 * 4|-$dat
  bmp=. xsbmp{.dat
else.
  pal=. ''
  spal=. 0
  if. -.if3 do.
    dat=. 256 256 256 #: dat
  end.
  dat=. flipwritergb"1 dat
  bmp=. ,"2 dat
  bmp=. ((1 4)* >. ($bmp) % (1 4)) {. bmp
end.

bmp=. ,|. bmp{ a.
sdat=. $bmp

j=. (|.sbmp),(256 #. 0, bit, 0 1),0,sdat,0 0,2#spal
b=. (54+(4*spal)+sdat),0,(54+4*spal),40,j
head=. 'BM',,a.{~,|."1 (4#256)#:b

(head,pal,bmp) 1!:2 boxopen file
)

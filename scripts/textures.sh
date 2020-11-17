
SCRIPTPATH="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

function texture {
  background=$1
  stripe=$2
  number=$3

  PS_CODE=$(cat <<EOF
%!PS
<</PageSize [1024 512]>> setpagedevice

% background
0 0 moveto 1024 0 rlineto 0 512 rlineto -1024 0 rlineto 0 -512 rlineto closepath
${background} setrgbcolor fill

% stripe
0 150 moveto 1024 0 rlineto 0 212 rlineto -1024 0 rlineto 0 -212 rlineto closepath
${stripe} setrgbcolor fill

% circles
256 256 80 0 360 arc
768 256 80 0 360 arc closepath
1 1 1 setrgbcolor fill

% text
/Rubik-Regular.ttf findfont 130 scalefont setfont
0 0 0 setrgbcolor
256 210 moveto
(${number}) stringwidth pop 2 div neg 0 rmoveto
(${number}) show
768 210 moveto
(${number}) stringwidth pop 2 div neg 0 rmoveto
(${number}) show

showpage
EOF
)

  echo "$PS_CODE" \
  | sed "s/$${background}/$background/g;s/$${stripe}/$stripe/g;s/$${number}/$number/g;" \
  | gs -I$SCRIPTPATH/font -dSAFER -dQUIET -dBATCH -dNOPAUSE -sDEVICE=png16m -dGraphicsAlphaBits=4 -dTextAlphaBits=4 -sOutputFile=- -\
  | pngquant --force --strip --speed=1 8 --output $SCRIPTPATH/../src/img/balls/$number.png -
}

texture "1 0.843 0" "1 0.843 0" 1 # yellow
texture "0 0 1" "0 0 1" 2 # blue
texture "1 0 0" "1 0 0" 3 # red
texture "0.294 0 0.509" "0.294 0 0.509" 4 # violet
texture "1 0.270 0" "1 0.270 0" 5 # orange
texture "0.133 0.545 0.133" "0.133 0.545 0.133" 6 # green
texture "0.5 0 0" "0.5 0 0" 7 # maroon
texture "0 0 0" "0 0 0" 8 # black
texture "1 1 1" "1 0.843 0" 9 # yellow-striped
texture "1 1 1" "0 0 1" 10 # blue-striped
texture "1 1 1" "1 0 0" 11 # red-striped
texture "1 1 1" "0.294 0 0.509" 12 # violet-striped
texture "1 1 1" "1 0.270 0" 13 # orange-striped
texture "1 1 1" "0.133 0.545 0.133" 14 # green-striped
texture "1 1 1" "0.5 0 0" 15 # maroon-striped

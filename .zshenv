# .zshenv


## 環境依存のファイル
#
if [ -e "$HOME/.zshrc.dep" ];then
    source $HOME/.zshrc.dep
fi


PATH=$HOME/.rbenv/shims:$HOME/.rbenv/bin:$PATH

if [ -e "$HOME/.rvm" ];then
    PATH=$HOME/.rvm/bin:$PATH # Add RVM to PATH for scripting
fi

export PERL_CPANM_OPT="--local-lib=~/perl5"
export PATH=$HOME/perl5/bin:$PATH;
export PERL5LIB=$HOME/perl5/lib/perl5:$PERL5LIB;

## RED と打つと標準エラーが赤になる
function redrev() {
    perl -pe 's/^/\e[41m/ && s/$/\e[m/';
}

alias -g RED='2> >(redrev)'


## 印刷コマンド
#
#alias PRINT='enscript --pretty-print --line-numbers --columns=2 --landscape --fancy-header'
PRINT(){
    nkf -e $1 |LANG=C a2ps -T 4 --line-numbers=1 --center-title=$1 --encoding=euc-jp
}

## 天気取得コマンド
alias tenki="ruby -r json -e 'p JSON.parse!(\`curl http://weather.livedoor.com/forecast/webservice/json/v1?city=130010\`)['\''forecasts'\''][1]['\''telop'\'']'"


function ej(){
    KEY=`echo $@ | nkf -w -w80`
    echo $@ >> ~/.eng-list # 履歴を保存（不要なら削除）．
    URI="http://eow.alc.co.jp/$KEY/UTF-8/"
    RS=`echo '▼ 検索結果本体 ▼' | nkf -w -w80`
    RE=`echo '▲ 検索結果本体 ▲' | nkf -w -w80`
    wget -q --referer='http://eow.alc.co.jp/'  -O - "$URI" | \
        sed -ne "/<!--\s*$RS\s*-->/,/<!--\s*$RE\s*-->/p" | w3m -dump -T 'text/html' | lv -c
}

# w3mでgoogle検索
function google() {
    local str opt
    if [ $ != 0 ]; then
        for i in $*; do
            str="$str+$i"
        done
        str=`echo $str | sed 's/^\+//'`
        opt='search?num=50&amp;hl=ja&amp;lr=lang_ja'
        opt="${opt}&amp;q=${str}"
    fi
    w3m http://www.google.co.jp/$opt
}

# w3mでALC検索
function alc() {
    if [ $ != 0 ]; then
        w3m "http://eow.alc.co.jp/$*/UTF-8/?ref=sa" |less
    else
        w3m "http://www.alc.co.jp/"
    fi
}


## PATHの設定
#
export PATH=${PATH}:~/bin

case "${OSTYPE}" in
freebsd*|darwin*)
export PATH=/usr/local/bin:/Applications:/usr/local/sbin:$PATH:
alias e='/usr/local/Cellar/emacs/24.3/bin/emacsclient'
alias emacs='/usr/local/Cellar/emacs/24.3/bin/emacs-24.3'

alias ls="ls -G -w"
;;
linux*)
alias ls="ls --color"
alias e='XMODIFIERS= /usr/bin/emacsclient'
alias emacs='XMODIFIERS= /usr/bin/emacs'
;;
cygwin*)
alias ls="ls --color"
alias e='/usr/local/bin/emacsclient'
;;
esac

## alias
#
alias myindent='indent -npsl -bap -bad -di16 -i4 -bli0  -ts255'
alias resource='source ~/.zshrc'

alias la="ls -la"
alias lf="ls -F"
alias ll="ls -l"
alias l="ls -l"

alias du="du -h"
alias df="df -h"

alias less='less -N --tabs=4'

#alias clean='rm -f *~ *.BAK *.bak '
alias cleanlog='rm -f *.log'

alias psx='ps auxww'
alias gps='ps auxww|grep'
alias KILL='kill -KILL '
alias fdump='od -xa '


## RVMの設定(Ruby)
#
if [ -z "$HOME/.rvm/scripts/rvm" ];then
    source $HOME/.rvm/scripts/rvm
fi

if which rbenv > /dev/null; then eval "$(rbenv init - zsh)"; fi

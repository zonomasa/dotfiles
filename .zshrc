#source $HOME/.zshrc.antigen


#http://bbeausej.tumblr.com/post/904302239/osx-bash-in-256-colors-my-zenburn-like-ls-colors
export LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'

## Environment variable configuration
#;
# LANG
#
export LANG=ja_JP.UTF-8


## プロンプトの設定
#
#
#
#################################################
# プロンプト表示フォーマット
# http://zsh.sourceforge.net/Doc/Release/zsh_12.html#SEC40
#################################################
# %% %を表示
# %) )を表示
# %l 端末名省略形
# %M ホスト名(FQDN)
# %m ホスト名(サブドメイン)
# %n ユーザー名
# %y 端末名
# %# rootなら#、他は%を表示
# %? 直前に実行したコマンドの結果コード
# %d ワーキングディレクトリ %/ でも可
# %~ ホームディレクトリからのパス
# %h ヒストリ番号 %! でも可
# %a The observed action, i.e. "logged on" or "logged off".
# %S (%s) 反転モードの開始/終了 %S abc %s とするとabcが反転
# %U (%u) 下線モードの開始/終了 %U abc %u とするとabcに下線
# %B (%b) 強調モードの開始/終了 %B abc %b とするとabcを強調
# %t 時刻表示(12時間単位、午前/午後つき) %@ でも可
# %T 時刻表示(24時間表示)
# %* 時刻表示(24時間表示秒付き)
# %w 日表示(dd) 日本語だと 曜日 日
# %W 年月日表示(mm/dd/yy)
# %D 年月日表示(yy-mm-dd)

autoload colors
colors

local GREEN=$'%{\e[38;5;106m%}'
#local GREEN=$'%{\e[0;32m%}'
local YELLOW=$'%{\e[38;5;186m%}'
local BLUE=$'%{\e[38;5;116m%}'
local RED=$'%{\e[38;5;169m%}'
local BROWN=$'%{\e[0;33m%}'
local DEFAULT=%{${reset_color}%}



# SVNのリビジョン番号を取得する
autoload -Uz add-zsh-hook
autoload -Uz colors
colors
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git svn hg bzr
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

autoload -Uz is-at-least
if is-at-least 4.3.10; then
  # この check-for-changes が今回の設定するところ
  zstyle ':vcs_info:git:*' check-for-changes true
  zstyle ':vcs_info:git:*' stagedstr "+"    # 適当な文字列に変更する
  zstyle ':vcs_info:git:*' unstagedstr "-"  # 適当の文字列に変更する
  zstyle ':vcs_info:git:*' formats '(%s)-[%b] %c%u'
  zstyle ':vcs_info:git:*' actionformats '(%s)-[%b|%a] %c%u'
fi

function _update_vcs_info_msg() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
add-zsh-hook precmd _update_vcs_info_msg


# プロンプトのフォーマット
# rootとそれ以外で分けるcase文
case ${UID} in
    0)
        PROMPT='['$RED'ROOT '$BROWN'%~'$DEFAULT']
$ %{%}%b' &&
        ;;
    *)
        PROMPT='['$RED'%n'$GREEN'@'$RED'%m '$GREEN'%~'$DEFAULT']
$ %{%}%b' &&
        ;;
esac

# コマンド修正時の確認メッセージ
SPROMPT="correct: %R -> %r ? " 

# 右側に出すプロンプト
RPROMPT="%1(v|%F{green}%1v%f|) --%W %T--"


# 補完機能
autoload -Uz compinit
compinit

## その他設定

# コマンドがディレクトリ時にcdする
setopt auto_cd

# cd時に自動的にpushdする
setopt auto_pushd

# ビープを使わない
setopt no_beep

# 自動修正機能
setopt correct

# 全引数に自動修正
setopt correct_all

# 曖昧な補完の時にビープ音を発しない
setopt no_list_beep

# 補完リストなるべく少ない行数に
setopt list_packed

# 補完候補を種別表示(ls -F)
setopt list_types

# 補完リストその他でもASCII(7ビット)以上の文字(8ビット)文字を表示 
# (マルチバイト文字補完)
setopt print_eight_bit

# プロンプトで変数拡張、コマンド置換、計算拡張が実行
setopt prompt_subst

# 同ディレクトリの複数のコピーをpushしない。
setopt pushd_ignore_dups

# コマンド履歴ファイルを共有する
setopt share_history

# 重複を記録しない
setopt hist_ignore_dups

# 開始と終了を記録
setopt extended_history

# emacs キーバインド
bindkey -e 


# 履歴ファイルの保存先
export HISTFILE=${HOME}/.zsh_history

# メモリに保存される履歴の件数
export HISTSIZE=10000

# 履歴ファイルに保存される履歴の件数
export SAVEHIST=100000


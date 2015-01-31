# 256 colors
function EXT_COLOR () { echo -ne "\[\033[38;5;$1m\]"; }

Orange="`EXT_COLOR 208`"
BYellow="\[\033[1;33m\]"
IBlack="\[\033[0;90m\]"
Green="\[\033[0;32m\]"

GIT_PROMPT_PREFIX="  " # start of the git info string
GIT_PROMPT_SUFFIX="" # the end of the git info string
GIT_PROMPT_SEPARATOR="" # separates each item
GIT_PROMPT_BRANCH="${Magenta}" # the git branch that is active in the current directory
GIT_PROMPT_STAGED=" ${Red}●" # the number of staged files/directories
GIT_PROMPT_CONFLICTS=" ${Red}✖" # the number of files in conflict
GIT_PROMPT_CHANGED=" ${Cyan}✚" # the number of changed files
GIT_PROMPT_REMOTE=" " # the remote branch name (if any)
GIT_PROMPT_UNTRACKED=" ${brown}…" # the number of untracked files/dirs
GIT_PROMPT_STASHED=" ${BoldBlue}" #⚑  	# the number of stashed files/dir
GIT_PROMPT_CLEAN=" ${Green}✔" # a colored flag indicating a "clean" repo
GIT_PROMPT_SYMBOLS_AHEAD='↑'
GIT_PROMPT_SYMBOLS_BEHIND='↓'
GIT_PROMPT_SYMBOLS_PREHASH=':'
GIT_PROMPT_START="${BoldBlue}\w:"
GIT_PROMPT_END="\n${BoldBlue}▶${ResetColor} "
GIT_PROMPT_LEADING_SPACE="0"

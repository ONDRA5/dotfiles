# If you come from bash you might have to change your $PATH.
#export PATH=$HOME/bin:/usr/local/bin:$PATH
PATH=$PATH:/home/ondra/.local/bin/
#export PATH=/home/ondra/.local/bin/

# Path to your oh-my-zsh installation.
export ZSH="/home/ondra/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="robbyrussell"

ZSH_THEME="spaceship" #settings values here: https://denysdovhan.com/spaceship-prompt/docs/Options.html#exit-code-exit_code

#	SPACESHIP SETTINGS             
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_CHAR_COLOR_SUCCESS=green
SPACESHIP_CHAR_COLOR_FAILURE=red
SPACESHIP_TIME_SHOW=false
SPACESHIP_USER_SHOW=false
SPACESHIP_PACKAGE_SHOW=true
#	DISABLE BLLSHIT
SPACESHIP_NODE_SHOW=false
SPACESHIP_RUBY_SHOW=false
SPACESHIP_ELM_SHOW=false
SPACESHIP_ELIXIR_SHOW=false
SPACESHIP_XCODE_SHOW=false
SPACESHIP_SWIFT_SHOW=false
SPACESHIP_GOLANG_SHOW=false
SPACESHIP_PHP_SHOW=false
SPACESHIP_RUST_SHOW=false
SPACESHIP_JULIA_SHOW=false
SPACESHIP_DOCKER_SHOW=false
SPACESHIP_DOCKER_CONTEXT_SHOW=false
SPACESHIP_AWS_SHOW=false
SPACESHIP_CONDA_SHOW=false
SPACESHIP_VENV_SHOW=false
SPACESHIP_PYENV_SHOW=false
SPACESHIP_DOTNET_SHOW=false
SPACESHIP_EMBER_SHOW=false
SPACESHIP_KUBECONTEXT_SHOW=false
SPACESHIP_TERRAFORM_SHOW=false
SPACESHIP_VI_MODE_SHOW=true
SPACESHIP_JOBS_SHOW=true

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
fi

# Compilation flags
export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
alias ls="lsd -hA --group-dirs first"
alias l="lsd -hA1 --group-dirs first"
alias v="nvim"
alias vim="nvim"
alias cp="cp -i"
alias mv="mv -i"
alias df="dfc"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias mail="mbsync -a && neomutt"
alias r="ranger"
alias clivid="mplayer -really-quiet -vo -caca" 
alias pipes="pipes -t 9"
alias config="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"
alias vifm="$HOME/.config/vifm/scripts/./vifmrun"

[[ -z $DISPLAY && XDG_VTNR -eq 1 ]] && exec startx

export TERMINAL='st'

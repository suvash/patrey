function fish_user_abbreviations
  set -U fish_user_abbreviations

    abbr ... '../..'
    abbr .... '../../..'

    abbr p 'fzf --preview \'if type -q bat; bat --color "always" {}; else; cat {}; end\''
    abbr l 'exa -la'
    abbr b 'bat --show-all'
    abbr v 'vim (fzf)'

    abbr ll 'ls -lah'
    abbr lsd 'tree --dirsfirst -ChF -L 1'

    abbr t 'tmux new -n (basename $PWD) -s (basename (dirname $PWD))"/"(basename $PWD)'
    abbr ta 'tmux attach'
    abbr tl 'tmux list-sessions'

    abbr ga 'git add'

    abbr gs 'git show --stat'
    abbr gst 'git status -sb'

    abbr gd 'git diff'
    abbr gdc 'git diff --cached'

    abbr gp 'git push'
    abbr gpf 'git push --force-with-lease'
    abbr gpu 'git push --set-upstream origin (__git_current_branch)'
    abbr gpd 'git push --delete origin (__git_current_branch)'

    abbr gt 'git branch -vv'

    abbr gsu 'git submodule update'

    abbr gup "git fetch --all -p; and git rebase -p '@{upstream}'"
    abbr gsp "git stash; and git fetch --all -p; and git rebase -p '@{upstream}'; and git stash pop"

    abbr gprn 'git remote prune origin --dry-run'

    abbr gm 'git merge --no-ff --log'
    abbr gc 'git commit -v'
    abbr gca 'git commit -v --amend'
    abbr gac 'git add -A; and git commit -v -m'

    abbr gco 'git checkout'
    abbr gcob 'git checkout -b'
    abbr gcm 'git checkout master'
    abbr gcr 'git checkout release'

    abbr gb 'git branch'
    abbr gbr 'git branch -r'
    abbr gbd 'git branch --delete'

    abbr gcp 'git cherry-pick'

    abbr gl 'git log --oneline --decorate=full'
    abbr gls 'git log --oneline --decorate=full --stat'
    abbr glg 'git log --graph --oneline --decorate=full'
    abbr glgs 'git log --graph --oneline --decorate=full --stat'

    abbr grh 'git reset HEAD'
    abbr grhh 'git reset HEAD --hard'
    abbr gcln 'git clean -f -d'

    abbr gdmlb 'git branch --merged | grep -v "\*" | xargs -n 1 git branch -d'

    abbr grbm 'git rebase -i (git show-branch --merge-base master)'

    abbr tf 'tail -f'
    abbr ps 'ps fauwwx'

    abbr ns 'nix-env -qaP \'*\' --description | grep'
    abbr ni "nix-env -iA nixpkgs.(hostname)"
    abbr nu 'nix-env -e'
    abbr ngc 'nix-collect-garbage'

    abbr fls 'fisher ls-remote --format="%stars - %name: %info https://%url\n" | sort -n'
    abbr fcln 'fisher ls | fisher rm; and rm -rf ~/.config/fisherman; and rm -rf ~/.cache/fisherman'

    abbr cleanhosts 'awk \'/HostName/{print $2}\'  ~/.ssh/config | xargs -r -n 1 ssh-keygen -R'
    abbr cleangoogle 'awk -F\',\' \'/googleusercontent/{print $1}\'  ~/.ssh/known_hosts | xargs -r -n 1 ssh-keygen -R'

    abbr mi "bash ~/Developer/scaffold/(hostname)/(hostname)_init.sh"
    abbr mb "bash ~/Developer/scaffold/(hostname)/(hostname)_build.sh"

    abbr di 'docker image ls'
    abbr dc 'docker container ls'
    abbr dn 'docker network ls'
    abbr dv 'docker volume ls'
    abbr ds 'docker system df'

    abbr emd 'emacs --daemon'
    abbr emt 'emacsclient -c -nw'
    abbr emc 'emacsclient -c -n'

    abbr p1 'ping 1.1.1.1'

    abbr export 'set -x'

    abbr portusedby 'lsof -i :'
    abbr allports 'netstat -tunap'

    abbr du 'du -chs *'
    abbr fonts 'fc-list : family'
    abbr xa 'xrandr --auto'
    abbr sp 'send_to_phone'
    abbr httpserve 'python -m http.server 7531'
end

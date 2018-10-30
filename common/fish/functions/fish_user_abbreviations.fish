function fish_user_abbreviations
  set -U fish_user_abbreviations \
    ...='../..' \
    ....='../../..' \
    \
    p='fzf --preview \'if type -q bat; bat --color "always" {}; else; cat {}; end\'' \
    l='exa -la' \
    v='vim (fzf)' \
    \
    ll='ls -lah' \
    lsd='tree --dirsfirst -ChF -L 1' \
    \
    t='tmux new -n (basename $PWD) -s (basename (dirname $PWD))"/"(basename $PWD)' \
    ta='tmux attach' \
    tl='tmux list-sessions' \
    \
    ga='git add' \
     \
    gs='git show' \
    gst='git status -sb' \
     \
    gd='git diff' \
    gdc='git diff --cached' \
     \
    gp='git push' \
    gpf='git push --force-with-lease' \
    gpu='git push --set-upstream origin (__git_current_branch)' \
    gpd='git push --delete origin (__git_current_branch)' \
    \
    gt='git branch -vv' \
     \
    gup="git fetch --all -p; and git rebase -p '@{upstream}'" \
    gsp="git stash; and git fetch --all -p; and git rebase -p '@{upstream}'; and git stash pop" \
     \
    gprn='git remote prune origin --dry-run' \
    \
    gm='git merge --no-ff --log' \
    gc='git commit -v' \
    gca='git commit -v --amend' \
    gac='git add -A; and git commit -v -m' \
     \
    gco='git checkout' \
    gcm='git checkout master' \
    \
    gb='git branch' \
    gbr='git branch -r' \
    gbd='git branch --delete' \
     \
    gcp='git cherry-pick' \
     \
    gl='git log --oneline --decorate=full' \
    gls='git log --oneline --decorate=full --stat' \
    glg='git log --graph --oneline --decorate=full' \
    glgs='git log --graph --oneline --decorate=full --stat' \
    \
    grh='git reset HEAD' \
    grhh='git reset HEAD --hard' \
    gcln='git clean -f -d' \
     \
    gdmlb='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d' \
    \
    grbm='git rebase -i (git show-branch --merge-base master)' \
    \
    tf='tail -f' \
    ps='ps fauwwx' \
    \
    ns='nix-env -qaP \'*\' --description | grep' \
    ni="nix-env -iA nixpkgs.(hostname)" \
    nu='nix-env -e' \
    ngc='nix-collect-garbage' \
    \
    fls='fisher ls-remote --format="%stars - %name: %info https://%url\n" | sort -n' \
    fcln='fisher ls | fisher rm; and rm -rf ~/.config/fisherman; and rm -rf ~/.cache/fisherman' \
    \
    cleanhosts='awk \'/HostName/{print $2}\'  ~/.ssh/config | xargs -r -n 1 ssh-keygen -R' \
    cleangoogle='awk -F\',\' \'/googleusercontent/{print $1}\'  ~/.ssh/known_hosts | xargs -r -n 1 ssh-keygen -R' \
    \
    mi="bash ~/Developer/scaffold/(hostname)/(hostname)_init.sh" \
    mb="bash ~/Developer/scaffold/(hostname)/(hostname)_build.sh" \
    \
    di='docker image ls' \
    dc='docker container ls' \
    dn='docker network ls' \
    dv='docker volume ls' \
    ds='docker system df' \
    \
    emd='emacs --daemon' \
    emt='emacsclient -c -nw' \
    emc='emacsclient -c -n' \
    \
    export='set -x' \
    \
    portusedby='lsof -i :' \
    allports='netstat -tunap' \
    \
    du='du -chs *' \
    fonts='fc-list' \
    xa='xrandr --auto' \
    sp='send_to_phone' \
    httpserve='python -m http.server 7531'
end

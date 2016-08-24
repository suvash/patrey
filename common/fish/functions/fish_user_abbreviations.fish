function fish_user_abbreviations
  set -U fish_user_abbreviations \
    ...='../..' \
    ....='../../..' \
    \
    l='ls -a' \
    ll='ls -lah' \
    lsd='tree --dirsfirst -ChF -L 1' \
    \
    t='tmux new -n (basename $PWD) -s (basename (dirname $PWD))"/"(basename $PWD)' \
    ta='tmux attach' \
    tl='tmux list-sessions' \
    \
    ga='git add' \
     \
    gst='git status -sb' \
    gs='git show' \
     \
    gd='git diff' \
    gdc='git diff --cached' \
     \
    gp='git push' \
    gpthis='git push origin HEAD:(__git_current_branch)' \
     \
    gup='git fetch origin; and git rebase -p origin/(__git_current_branch)' \
    gsp='git stash; and git fetch origin; and git rebase -p origin/(__git_current_branch); and git stash pop' \
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
    gcd='git checkout develop' \
     \
    gb='git branch' \
    gba='git branch -a' \
    gbr='git branch -r' \
    gbd='git branch -D' \
     \
    gcp='git cherry-pick' \
     \
    gl='git log' \
    gls='git log --stat --max-count=5' \
    glg='git lg --max-count=5' \
    glgg='git log --graph --max-count=5' \
     \
    gcf='git diff-tree --no-commit-id --name-only -r HEAD' \
     \
    grh='git reset HEAD' \
    grhh='git reset HEAD --hard' \
    gcln='git clean -f -d' \
     \
    gdmlb='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d' \
    \
    grbb='git rebase -i (git show-branch --merge-base master)' \
    \
    bi='bundle install --jobs 3 --path .bundle/gems --binstubs .bundle/bin' \
    ber='bundle exec rake' \
    be='bundle exec' \
    t='tig' \
    tf='tail -f' \
    ps='ps auwwx' \
    \
    ns='nix-env -qaP \'*\' --description | grep' \
    ni="nix-env -iA nixpkgs.(hostname)" \
    nu='nix-env -e' \
    ngc='nix-collect-garbage' \
    \
    vu='vagrant up' \
    vd='vagrant destroy -f' \
    vgs='vagrant global-status --prune' \
    vha='vagrant global-status --prune | awk \'/running/{print $1}\' | xargs -n 1 -- vagrant halt' \
    \
    cleanhosts='awk \'/HostName/{print $2}\'  ~/.ssh/config | xargs -n 1 ssh-keygen -R' \
    \
    mi="bash ~/Developer/scaffold/(hostname)/(hostname)_init.sh" \
    mb="bash ~/Developer/scaffold/(hostname)/(hostname)_build.sh" \
    \
    v='vim' \
    emd='emacs --daemon' \
    emt='emacsclient -c -nw' \
    emc='emacsclient -c -n' \
    \
    cmprs7z='7za a -t7z -mx=9 -m0=lzma2 -ms=on -mhc=on -mhe=on -p' \
    cmprszip='7za a -tzip -mx=9 -mem=aes256 -p' \
    \
    ducks='du -cks * | sort -rn | head' \
    cl='tty-clock' \
    fonts='fc-list' \
    xa='xrandr --auto' \
    tb='xrandr --output eDP1 --mode 1440x900 --pos 0x0 --rotate normal --output DP1 --mode 2560x1440 --pos 1440x0 --rotate normal' \
    wm='xrandr --output eDP1 --primary --mode 1440x900 --pos 240x1200 --rotate normal --output HDMI1 --mode 1920x1200 --pos 0x0 --rotate normal' \
    sp='send_to_phone' \
    httpserve='python -m SimpleHTTPServer 8000'
end

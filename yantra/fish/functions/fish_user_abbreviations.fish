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
    \
    v='vim' \
    emd='emacs --daemon' \
    em='emacsclient -nw' \
    emc='emacsclient -c -nw' \
    emg='emacsclient -c -n' \
    \
    ducks='du -cks * | sort -rn | head' \
    sp='send_to_phone'
end

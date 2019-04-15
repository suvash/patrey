function cleanip
    awk -F',' "/$argv/"'{print $1}'  ~/.ssh/known_hosts | xargs -r -n 1 ssh-keygen -R
end

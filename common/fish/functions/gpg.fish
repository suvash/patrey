function gpg
  if command -v gpg2 > /dev/null ^ /dev/null
    command gpg2 $argv
  else if command -v gpg > /dev/null ^ /dev/null
    command gpg $argv
  else
    echo 'Neither gpg2 nor gpg was found in $PATH.'
  end
end

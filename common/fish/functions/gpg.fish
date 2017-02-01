function gpg
  if command -s gpg2 > /dev/null ^ /dev/null
    command gpg2 $argv
  else if command -s gpg > /dev/null ^ /dev/null
    command gpg $argv
  else
    echo 'Neither gpg2 nor gpg was found in $PATH.'
  end
end

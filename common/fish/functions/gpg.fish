function gpg
  switch (uname)
    case Darwin
      command gpg $argv
    case Linux
      if type gpg2 > /dev/null ^ /dev/null
        command gpg2 $argv
      else
        command gpg $argv
      end
  end
end

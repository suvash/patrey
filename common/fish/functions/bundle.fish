function bundle
	if test -f $PWD/Vagrantfile
		echo 'Running bundle inside vagrant...'
		set newargv (echo $argv | awk -v PWD=$PWD'/' '{ gsub(PWD,"",$4); print $0}')
		echo '=> bundle '$newargv
		vagrant ssh -c "cd /vagrant; bundle $newargv"
	else
		command bundle $argv
	end
end


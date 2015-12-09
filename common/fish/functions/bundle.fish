function bundle
	if test -f $PWD/Vagrantfile
		echo 'Running bundle inside vagrant...'
		set argv1 (echo $argv | awk -v SPEC=' SPEC_OPTS=--format documentation ' '{ gsub(SPEC," ",$0); print $0}')
		set argv2 (echo $argv1 | awk -v PWD=$PWD'/' '{ gsub(PWD,"",$0); print $0}')
		echo "=> bundle $argv2"
		vagrant ssh -c "cd /vagrant; bundle $argv2"
	else
		command bundle $argv
	end
end


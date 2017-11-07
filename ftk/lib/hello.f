       program hello

	open(111,file='hello.out')
	write(111,*) 'hello world'
	close(111)

	end

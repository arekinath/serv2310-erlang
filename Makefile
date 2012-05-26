all:
	cd ebin; erlc ../src/*
clean:
	rm -fr ebin/*

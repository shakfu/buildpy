
with open('cfg') as f:
	lines = [line for line in f.readlines() if line]
	res = {}
	for line in lines:
		head, *tail = line.split()
		res[head] = tail

	print(res)

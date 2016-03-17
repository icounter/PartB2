
all:
	nvcc -o cuda-thrust cuda-thrust.cu
  
clean:
	rm cuda-thrust

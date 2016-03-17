t.cu line 24:
  return (((p.x-0.5)*(p.x-0.5)+(p.y-0.5)*(p.y-0.5))<0.25) ? 1 : 0;             
1. cuda_thrust.cu line 1&2
   The 2 includes should not be comented
2. cuda_thrust.cu line 28
    It should be __device__ not **device**
3. cuda_thrust.pbs line 4
    ncpus=1
4. cuda_thrust.pbs line 18
    ./cuda-thrust
5. makefile line 5
    source should be cuda-thrust.cu not cuda-thrust.c
6. uncomment in cuda-thrust.pbs
cd $PBS_O_WORKDIR

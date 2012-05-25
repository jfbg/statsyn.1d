program sumupkernels


			integer				:: status, nlines
			real					:: x, z, t, v1, V 
			
			
			
			
			
			
			do I = 1,nlines
				
				if (I == 1)
					read(1,*.IOSTAT=status) x z t v1
					V = v
				  write(2,FMT=788) x z t V
				else
					read(1,*.IOSTAT=status) x z t v1
					read(2,*.IOSTAT=status) x z t V
					V = V+v1
					write(2,FMT=788) x z t V
		
		
788   FORMAT(3(f10.2,1X),f15.5) 

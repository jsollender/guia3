module interp

use iso
implicit none

contains

	subroutine lagrange(n, x, fx, c, pc)
        integer(il), intent(in)                  ::  n  !grado del polinomio
        real(wp), dimension(0:n), intent(in)     :: x  !vector de los xi
        real(wp), dimension(0:n), intent(in)     :: fx !vector de los fxi
        real(wp), intent(in)                     :: c   !punto a evaluar
        real(wp), intent(out)                    :: pc  !polinomio evaluado c

        !variables auxiliares
        real(wp)                    :: lci
        integer(il)                 :: i, k
        !-----------------------------------------------------
        !Proceso
        pc = 0.0_wp
        !auxl = (c - x(i)) / (x(k)- x(i))
        do k = 0, n
            lci = 1.0_wp
            do i = 0, n
                if (k /= i) then
                    lci = lci * (c - x(i)) / (x(k)- x(i))
                end if

            end do
            pc = pc + lci * fx(k)
        end do

    	end subroutine lagrange


	subroutine newton
	
	!variabkes qe ebtrab
	!grado n, xi, fxi, el x (el c)
	!sale el pol en c
	
	integer (il), intent (in)				::	n
	real (wp), intent (in)					::	c
	real (wp), dimension (0:n), intent (in)	::	x, fx
	real (wp), intent (out)					::	pc
	
!-----------------------------------------------------

	integer (il)					::	i, j !fila columna
	real (wp)						::	aux
	real (wp), dimension (0:n,0:n)	::	matriz = 0.0_wp
	
	do i = 0, n
		matriz (i,0) = fx(i)	
	end do
	
	
	do j = 1, n
		
		do i = j, n
		matriz (i,j) = ((matriz (i, j-1) - matriz(i-1, j-1))/x(i)-x(i-j))
		!M(n,n) = M(n,n-1)-M(n-1,n-1)/xn-x0 
		end do
	
	end do
	
	pc	= matriz(0,0)
	
	do i = 1, n
		aux	= 1._wp
		
		do j = 1, i
			aux = aux*(c-x(j-1))  
		end do
		
		pc = pc + matriz(i,i) * aux
	end do
	
	
	end subroutine newton

end module interp

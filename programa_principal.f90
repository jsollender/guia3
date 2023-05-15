program principal

	use iso
	use modulo_interpolacion_newton
	use !no me acuerdo el nombre :(

	implicit none
	
	!declaración de variables
	
	real(wp)					::	c, pc, func, gunc
	integer (il)				::	n, i
	real(wp), dimension(0:2)	::	x, fx	!acá pide sí o sí un numero. no c por ke.
	
	interface
		function func(x) result (fx)
			implicit none
			real(wp), intent (in)	::	x
			real(wp), intent (out)	::	fx	
			end function
	
		function gunc(x) result (fx)
			implicit none
			real(wp), intent (in)	::	x
			real(wp), intent (out)	::	fx	
			end function
	end interface
	
	n = 1
	
	x(0) = 0._wp 	
	x(1) = 0.6_wp
	x(2) = 0.9_wp
	
	c = 0.45
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.	
!Newton
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = func(x(i))
	end do
	
	call modulo_interpolacion_newton (n, x, fx, c, pc)
	print (*,*) "el pol para f segun newton", pc
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = gunc(x(i))
	end do
	
	call modulo_interpolacion_newton (n, x, fx, c, pc)
	print (*,*) "el pol para g segun newton", pc
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.	
!Lagrange
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = func(x(i))
	end do
	
	call InserteNombreDelModuloDeLagrange (n, x, fx, c, pc)
	print (*,*) "el pol para f segun Lagrange", pc
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = gunc(x(i))
	end do
	
	call InserteNombreDelModuloDeLagrange (n, x, fx, c, pc)
	print (*,*) "el pol para g segun Lagrange", pc
	
end program principal

function func(x) result (fx)
	implicit none
	real(wp), intent (in)	::	x
	real(wp), intent (out)	::	fx	
	fx = log(x + 1)
end function

function gunc(x) result (fx)
	implicit none
	real(wp), intent (in)	::	x
	real(wp), intent (out)	::	fx	
	fx = sqrt(x + 1)
end function

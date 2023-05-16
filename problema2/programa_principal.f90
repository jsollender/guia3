program principal

	use mod_prec
	use interp !newton
	use interpolacion !lagrange
	implicit none
	
	!declaración de variables
	
	real(wp)			::	c, pc, func, gunc
	integer (il)			::	n, i
	real(wp), dimension(0:2)	::	x, fx !acá pide sí o sí un numero. no c por ke.
	
	interface
		function func(x)
			use mod_prec
			implicit none
			real(wp), intent (in)	::	x
			real(wp)		::	fx	
			end function
	
		function gunc(x)
			use mod_prec
			implicit none
			real(wp), intent (in)	::	x
			real(wp)		::	fx	
			end function
	end interface
	
	n = 1
	
	x(0) = 0._wp 	
	x(1) = 0.6_wp
	x(2) = 0.9_wp
	
	c = 0.45_wp
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.	
!Newton
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = func(x(i))
	end do
	
	call newton (n, x, fx, c, pc)
	write (*,*) "el pol para f segun newton", pc
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = gunc(x(i))
	end do
	
	call newton (n, x, fx, c, pc)
	write (*,*) "el pol para g segun newton", pc
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.	
!Lagrange
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = func(x(i))
	end do
	
	call lagrange (n, x, fx, c, pc)
	write (*,*) "el pol para f segun Lagrange", pc
	
!.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.
	
	do i = 0, n
		fx(i) = gunc(x(i))
	end do
	
	call lagrange (n, x, fx, c, pc)
	write (*,*) "el pol para g segun Lagrange", pc
	
end program principal

function func(x) result (fx)
	use mod_prec
	implicit none
	real(wp), intent (in)	::	x
	real(wp)		::	fx	
	fx = log(x + 1)
end function

function gunc(x) result (fx)
	use mod_prec
	implicit none
	real(wp), intent (in)	::	x
	real(wp)		::	fx	
	fx = sqrt(x + 1)
end function

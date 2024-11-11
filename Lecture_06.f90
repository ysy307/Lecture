program Lecture_06
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32) :: num_cases

    write(*,*) "Enter the problem number:"
    read(*,*) num_cases

    select case(num_cases)
        case (1)
            block
                real(real64), allocatable :: x(:), y(:)
                real(real64)   :: h
                integer(int32) :: i
                integer(int32) :: n, nunit
                character(100) :: filename

                n = 10
                h = 1.0d0 / dble(n)

                call Fix_InitialCondition(x, y, h, n, 0.0d0, 0.0d0)
                call Euler_Forward(f61, x, y, h)
                
                write(filename, '(a,i0,a)') 'result/L6/61_Euler_', n, '.dat'
                open(newunit=nunit, file=filename, status='replace')
                write(nunit,'(a)') "x   y   Exact"
                do i = 1, n + 1
                    write(nunit,'(3es20.12)') x(i), y(i), f61_Exact(x(i))
                end do
                close(nunit)
            end block
        case (2)
            block
                real(real64), allocatable :: x(:), y(:)
                real(real64)   :: h
                integer(int32) :: i
                integer(int32) :: n, nunit
                character(100) :: filename

                write(*,*) "Enter the number of steps:"
                read(*,*) n
                if (n <= 0) then
                    write(*,*) "Invalid number of steps."
                    stop
                end if

                h = 1.0d0 / dble(n)

                call Fix_InitialCondition(x, y, h, n, 0.0d0, 1.0d0)
                call Euler_Forward(f62, x, y, h)
                
                write(filename, '(a,i0,a)') 'result/L6/62_Euler_', n, '.dat'
                open(newunit=nunit, file=filename, status='replace')
                write(nunit,'(a)') "x   y   Exact"
                do i = 1, n + 1
                    write(nunit,'(3es20.12)') x(i), y(i), f62_Exact(x(i))
                end do
                close(nunit)

            end block
        case (3)
            block
                real(real64), allocatable :: x(:), y(:)
                real(real64)   :: h
                integer(int32) :: i
                integer(int32) :: n, nunit
                character(100) :: filename

                write(*,*) "Enter the number of steps:"
                read(*,*) n
                if (n <= 0) then
                    write(*,*) "Invalid number of steps."
                    stop
                end if

                h = 20.0d0 / dble(n)

                call Fix_InitialCondition(x, y, h, n, 0.0d0, 1.0d-1)
                call Euler_Forward(f63, x, y, h)
                
                write(filename, '(a,i0,a)') 'result/L6/63_Euler_', n, '.dat'
                open(newunit=nunit, file=filename, status='replace')
                write(nunit,'(a)') "x   y   Exact"
                do i = 1, n + 1
                    write(nunit,'(3es20.12)') x(i), y(i), f63_Exact(x(i))
                end do
                close(nunit)

            end block
        case (4)
            block

            end block
        case (5)
            block

            end block
        case(6)
            block

            end block
        case(7)
            block

            end block
        case(8)
            block

            end block
        case (:0)
            write(*,*) "Invalid problem number."
        case (9:)
            write(*,*) "Invalid problem number."
    end select

contains
    double precision function f61(x, y)
        implicit none
        double precision, intent(in) :: x, y
        f61 = x + y
    end function f61

    double precision function f61_Exact(x)
        implicit none
        double precision, intent(in) :: x
        f61_Exact = -x + exp(x) - 1.0d0
    end function f61_Exact

    double precision function f62(x, y)
        implicit none
        double precision, intent(in) :: x, y
        f62 = y + x**2.0d0 - 2.0d0 * x
    end function f62

    double precision function f62_Exact(x)
        implicit none
        double precision, intent(in) :: x
        f62_Exact = exp(x) - x**2.0d0
    end function f62_Exact

    double precision function f63(x, y)
        implicit none
        double precision, intent(in) :: x, y
        f63 = y - y**2.0d0
    end function f63

    double precision function f63_Exact(x)
        implicit none
        double precision, intent(in) :: x
        f63_Exact = exp(x) / (9.0d0 + exp(x))
    end function f63_Exact

    subroutine Fix_InitialCondition(x, y, h, n, x0, y0)
        implicit none
        real(real64), intent(inout), allocatable :: x(:), y(:)
        real(real64), intent(in)                 :: h, x0, y0
        integer(int32), intent(in)               :: n

        integer(int32) :: i

        if (.not. allocated(x)) then
            allocate(x(n+1))
        end if
        if (.not. allocated(y)) then
            allocate(y(n+1))
        end if

        x(1) = x0
        y(1) = y0

        do i = 1, n
            x(i + 1) = x(i) + h
        end do

    end subroutine Fix_InitialCondition

    subroutine Euler_Forward(f, x, y, h)
        implicit none
        interface
            double precision function f(x, y)
                double precision, intent(in) :: x, y
            end function f
        end interface
        real(real64), intent(in)    :: x(:)
        real(real64), intent(in)    :: h
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i

        do i = 1, size(x) - 1
            y(i + 1) = y(i) + h * f(x(i + 1), y(i))
        end do

    end subroutine Euler_Forward


end program Lecture_06
program Lecture_03
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32) :: num_cases
    real(real64)   :: pi

    pi = 4.0d0 * atan(1.0d0)

    write(*,*) "Enter the problem number:"
    read(*,*) num_cases

    select case(num_cases)
        case (1)
            block
                integer(int32), parameter :: N = 8
                real(real64) :: Is, Ie
                real(real64) :: Int

                Is = 0.0d0
                Ie = pi / 2.0d0

                Int = Integrate_trapezoidal(F_sin, Is, Ie, N)
                write(*,*) "The integral of sin(x) from 0 to pi/2 is ", Int
                write(*,'(a,es15.7)') "Error is", 1.0d0 - Int
            end block
        case (2)
            block
                integer(int32), parameter :: N = 8
                real(real64) :: Is, Ie
                real(real64) :: Int

                Is = 0.0d0
                Ie = pi / 2.0d0

                Int = Integrate_Simpson(F_sin, Is, Ie, N)
                write(*,*) "The integral of sin(x) from 0 to pi/2 is ", Int
                write(*,'(a,es15.7)') "Error is", 1.0d0 - Int
            end block
        case (3)
            block
                integer(int32) :: N
                real(real64)   :: Is, Ie
                real(real64)   :: Int

                write(*,*) "Enter even division number (N):"
                read(*,*)  N

                Is = 0.0d0
                Ie = 1.0d0

                Int = Integrate_Simpson(F_Pi, Is, Ie, N)
                write(*,*) "The integral is ", Int
                write(*,'(a,es15.7)') "Error is", Int - Pi
            end block
        case (4)
            block
                integer(int32) :: N(4)
                integer(int32) :: iN
                real(real64)   :: Is, Ie
                real(real64)   :: Int
                real(real64)   :: Analytic

                N(:) = [8, 16, 32, 64]
                Analytic = 32.0d0 / 3.0d0

                Is = 0.0d0
                Ie = 2.0d0

                do iN = 1, size(N)
                    Int = Integrate_Simpson(F_Power, Is, Ie, N(iN))
                    write(*,'(a,i0)') "Number of Division: ", N(iN)
                    write(*,*) "The integral is ", Int
                    write(*,'(a,es15.7)') "Error is", Int - Analytic
                end do
            end block
        case (5)
            block
                integer(int32) :: N
                real(real64)   :: Is, Ie
                real(real64)   :: Int
                real(real64)   :: Analytic

                N = 3
                Analytic = 32.0d0 / 3.0d0

                Is = 0.0d0
                Ie = 2.0d0

                Int = Integrate_Gaussian_Quadrature(F_Power, Is, Ie, N)
                write(*,'(a,i0)') "Number of Division: ", N
                write(*,*) "The integral is ", Int
                write(*,'(a,es15.7)') "Error is", Int - Analytic

            end block
        case(6)
            block
                integer(int32) :: N(4)
                integer(int32) :: iN, newnuit
                real(real64)   :: Is, Ie
                real(real64)   :: Int
                real(real64)   :: Analytic

                N(:) = [8, 12, 16, 20]
                Analytic = pi / 2.0d0

                Is = -1.0d0
                Ie = 1.0d0

                open(unit=newnuit, file="result/L3/Quadrature.dat", status='replace')
                do iN = 1, size(N)
                    Int = Integrate_trapezoidal(F_Root, Is, Ie, N(iN))
                    write(newnuit,'(a,i0)') "Trapezoidal| Number of Division: ", N(iN)
                    write(newnuit,*) "The integral is ", Int
                    write(newnuit,'(a,es15.7)') "Error is", abs(Int - Analytic)
                    Int = Integrate_Simpson(F_Root, Is, Ie, N(iN))
                    write(newnuit,'(a,i0)') "Simpson| Number of Division: ", N(iN)
                    write(newnuit,*) "The integral is ", Int
                    write(newnuit,'(a,es15.7)') "Error is", abs(Int - Analytic)
                    Int = Integrate_Gaussian_Quadrature(F_Root, Is, Ie, iN)
                    write(newnuit,'(a,i0)') "Gaussian_Quadrature| Number of Division: ", iN
                    write(newnuit,*) "The integral is ", Int
                    write(newnuit,'(a,es15.7)') "Error is", abs(Int - Analytic)
                    write(newnuit,*) ""
                end do
                close(newnuit)
            end block
        case (:0)
            write(*,*) "Invalid problem number."
        case (7:)
            write(*,*) "Invalid problem number."
    end select

contains

    double precision function F_sin(x)
        double precision, intent(in) :: x
        F_sin = sin(x)
    end function F_sin

    double precision function F_Pi(x)
        double precision, intent(in) :: x
        F_Pi = 4.0d0 / (1.0d0 + x**2)
    end function
    
    double precision function F_Power(x)
        double precision, intent(in) :: x
        F_Power = x**5
    end function

    double precision function F_Root(x)
        double precision, intent(in) :: x
        F_Root = sqrt(1 - x**2)
    end function

    real(real64) function Integrate_trapezoidal(f, a, b, N)
        implicit none
        interface
            double precision function f(x)
                double precision, intent(in) :: x
            end function f
        end interface
        real(real64), intent(in)   :: a, b
        integer(int32), intent(in) :: N
        real(real64)               :: h, sum, x
        integer(int32)             :: i

        h = (b - a) / N
        sum = 0.5d0 * (f(a) + f(b))
        do i = 1, N - 1
            x = a + h * dble(i)
            sum = sum + f(x)
        end do
        sum = sum * h
        Integrate_trapezoidal = sum
    end function Integrate_trapezoidal

    real(real64) function Integrate_Simpson(f, a, b, N)
        implicit none
        interface
            double precision function f(x)
                double precision, intent(in) :: x
            end function f
        end interface
        real(real64), intent(in)   :: a, b
        integer(int32), intent(in) :: N
        real(real64)               :: h, sum, x
        integer(int32)             :: i

        if (mod(N, 2) /= 0) then
            write(*,*) "Error: N must be even for Simpson's rule."
            Integrate_Simpson = 0.0d0
            return
        end if

        h = (b - a) / dble(N)

        sum = f(a) + f(b)
        do i = 1, N - 1
            x = a + h * dble(i)
            if (mod(i, 2) == 0) then
                sum = sum + 2.0d0 * f(x)
            else
                sum = sum + 4.0d0 * f(x)
            end if
        end do

        sum = sum * h / 3.0d0
        Integrate_Simpson = sum
    end function Integrate_Simpson

    real(real64) function Integrate_Gaussian_Quadrature(f, a, b, N)
        implicit none
        interface
            double precision function f(x)
                double precision, intent(in) :: x
            end function f
        end interface
        real(real64), intent(in)   :: a, b
        integer(int32), intent(in) :: N
        real(real64)               :: sum
        real(real64), allocatable  :: weights(:), points(:)

        select case (N)
        case (1)
            weights = [2.0d0]
            points  = [0.0d0]
        case (2)
            weights = [1.0d0, 1.0d0]
            points  = [-sqrt(1.0d0/3.0d0), sqrt(1.0d0/3.0d0)]
        case (3)
            weights = [5.0d0/9.0d0, 8.0d0/9.0d0, 5.0d0/9.0d0]
            points  = [-sqrt(0.6d0), 0.0d0, sqrt(0.6d0)]
        case (4)
            weights = [(18.0d0 + sqrt(30.0d0)) / 36.0d0, (18.0d0 + sqrt(30.0d0)) / 36.0d0, &
                       (18.0d0 - sqrt(30.0d0)) / 36.0d0, (18.0d0 - sqrt(30.0d0)) / 36.0d0]
            points  = [sqrt((3.0d0 - 2.0d0 * sqrt(1.2d0)) / 7.0d0), -sqrt((3.0d0 - 2.0d0 * sqrt(1.2d0)) / 7.0d0), &
                       sqrt((3.0d0 + 2.0d0 * sqrt(1.2d0)) / 7.0d0), -sqrt((3.0d0 + 2.0d0 * sqrt(1.2d0)) / 7.0d0)]
        case (5)
            weights = [128.0d0 / 225.0d0, (322.0d0 + 13.0d0 * sqrt(70.0d0)) / 900.0d0, &
                       (322.0d0 + 13.0d0 * sqrt(70.0d0)) / 900.0d0, (322.0d0 - 13.0d0 * sqrt(70.0d0)) / 900.0d0, &
                       (322.0d0 - 13.0d0 * sqrt(70.0d0)) / 900.0d0]
            points  = [0.0d0, sqrt(5.0d0 - 2.0d0 * sqrt(10.0d0 / 7.0d0)) / 3.0d0, &
                       -sqrt(5.0d0 - 2.0d0 * sqrt(10.0d0 / 7.0d0)) / 3.0d0, sqrt(5.0d0 + 2.0d0 * sqrt(10.0d0 / 7.0d0)) / 3.0d0, &
                       -sqrt(5.0d0 + 2.0d0 * sqrt(10.0d0 / 7.0d0)) / 3.0d0]
        case default
            write(*,*) "Error: Order N out of implemented range (1 to 5)."
            stop
        end select

        sum = GQ_Inner_Integration(f, a, b, weights, points)
        Integrate_Gaussian_Quadrature = sum
    end function Integrate_Gaussian_Quadrature

    real(real64) function GQ_Inner_Integration(f, a, b, weights, points)
        implicit none
        interface
            double precision function f(x)
                double precision, intent(in) :: x
            end function f
        end interface
        real(real64), intent(in)   :: a, b
        real(real64), intent(in)   :: weights(:), points(:)
        integer(int32)             :: i
        real(real64)               :: sum

        sum = 0.0d0
        do i = 1, size(weights)
            sum = sum + weights(i) * f((b - a) * points(i) / 2.0d0 + (a + b) / 2.0d0)
        end do
        GQ_Inner_Integration = sum * (b - a) / 2.0d0
    end function GQ_Inner_Integration



end program Lecture_03

program Lecture_04
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
                integer(int32), parameter :: maxIter = 100000
                real(real64) :: a, b
                real(real64) :: convergence, epsilon
                real(real64) :: result_BS, result_NM


                convergence = 1.0d-10
                epsilon     = 0.5d-5

                a = 0.0d0
                b = 1.0d0

                result_BS = Binary_search(F_xcos, a, b, convergence, epsilon, maxIter)
                write(*,'(a,f13.10)') "Binary search       : ",result_BS
                result_NM = Newton_Method_First(F_xcos, DF_xcos, a, convergence, maxIter)
                write(*,'(a,f13.10)') "Newton Method       : ",result_NM
                write(*,'(a,es8.1)')  "Convergence delta   : ",convergence 
                write(*,'(a,es8.1)')  "Convergence epsilon : ",epsilon
            end block
        case (2)
            block
                integer(int32), parameter :: maxIter = 100000
                real(real64) :: a
                real(real64) :: convergence
                real(real64) :: result_NM


                convergence = 1.0d-10

                a = 0.0d0

                result_NM = Newton_Method_First(F_Poly_Third, DF_Poly_Third, a, convergence, maxIter)
                write(*,'(a,f13.10)') "Newton Method       : ", result_NM
                write(*,'(a,es8.1)')  "Convergence delta   : ",convergence 

            end block
        case (3)
            block
                integer(int32), parameter :: maxIter = 100000
                real(real64) :: a
                real(real64) :: convergence
                real(real64) :: result_NM


                convergence = 1.0d-10

                a = -2.0d0

                result_NM = Newton_Method_First(F_xsin, DF_xsin, a, convergence, maxIter)

                write(*,'(a,f13.10)') "Initial value       : ", a
                write(*,'(a,f13.10)') "Newton Method       : ", result_NM
                write(*,'(a,es8.1)')  "Convergence delta   : ",convergence 
                end block
        case (4)
            block
                integer(int32), parameter :: maxIter = 100000
                real(real64) :: a, b
                real(real64) :: convergence, epsilon
                real(real64) :: result_BS


                convergence = 1.0d-10   
                epsilon     = 0.5d-5

                a = -3.0d0
                b =  0.0d0

                result_BS = Binary_search(F_Poly_Third_F, a, b, convergence, epsilon, maxIter)
                write(*,'(a,f13.10)') "Binary search       : ",result_BS
                write(*,'(a,es8.1)')  "Convergence delta   : ",convergence 
                write(*,'(a,es8.1)')  "Convergence epsilon : ",epsilon
            end block
        case (5)
            block
                integer(int32), parameter :: maxIter = 100000
                real(real64) :: a
                real(real64) :: convergence
                real(real64) :: result_NM


                convergence = 1.0d-10

                a = 0.0d0

                result_NM = Newton_Method_First(F_Poly_Third_F, DF_Poly_Third_F, a, convergence, maxIter)

                write(*,'(a,f13.10)') "Newton Method       : ", result_NM
                write(*,'(a,es8.1)')  "Convergence delta   : ",convergence 
            end block
        case(6)
            block
                integer(int32), parameter :: maxIter = 100000
                real(real64) :: a, b
                real(real64) :: convergence
                real(real64) :: result_NM_x, result_NM_y
                integer(int32) :: status


                convergence = 1.0d-10

                a = -1.0d0
                b = -2.0d0

                call Newton_Method_2D(F, G, Fx, Fy, Gx, Gy, a, b, convergence, maxIter, result_NM_x, result_NM_y, status)

                if (status == 0) then
                    write(*,'(a,f13.10,x,f13.10)') "Initial value     : ", a, b
                    write(*,'(a,f13.10,x,f13.10)') "Newton Method     : ", result_NM_x, result_NM_y
                    write(*,'(a,es8.1)')  "Convergence delta : ",convergence 
                else
                    write(*,'(a)') "Warning: Convergence conditions not reached"
                end if
            end block
        case(7)
            block
                integer(int32), parameter :: maxIter = 100000
                real(real64) :: a, b
                real(real64) :: convergence
                real(real64) :: result_NM_x, result_NM_y
                integer(int32) :: status


                convergence = 1.0d-10

                a = -1.0d0
                b = -3.0d0

                call Newton_Method_2D(F, G, Fx, Fy, Gx, Gy, a, b, convergence, maxIter, result_NM_x, result_NM_y, status)

                if (status == 0) then
                    write(*,'(a,f13.10,x,f13.10)')   "Initial value     : ", a, b
                    if (result_NM_y < 0.0d0) then
                        write(*,'(a,2f13.10,a)') "Newton Method    z= ", result_NM_x, result_NM_y, "i"
                    else
                        write(*,'(a,f13.10,a,f13.10,a)') "Newton Method    z= ", result_NM_x, "+", result_NM_y, "i"
                    end if
                    write(*,'(a,es8.1)')  "Convergence delta : ",convergence 
                else
                    write(*,'(a)') "Warning: Convergence conditions not reached"
                end if
            end block
        case (:0)
            write(*,*) "Invalid problem number."
        case (8:)
            write(*,*) "Invalid problem number."
    end select

contains

    double precision function F_xcos(x)
        double precision, intent(in) :: x
        F_xcos = x - cos(x)
    end function F_xcos

    double precision function DF_xcos(x)
        double precision, intent(in) :: x
        DF_xcos = 1 + sin(x)
    end function DF_xcos

    double precision function F_Poly_Third(x)
        double precision, intent(in) :: x
        F_Poly_Third = x**3.0d0 + x - 1.0d0
    end function F_Poly_Third

    double precision function DF_Poly_Third(x)
        double precision, intent(in) :: x
        DF_Poly_Third = 3.0d0 * x**2.0d0 + 1.0d0
    end function DF_Poly_Third

    double precision function F_Poly_Third_F(x)
        double precision, intent(in) :: x
        F_Poly_Third_F = x**3.0d0 + 6.0d0 * x**2.0d0 + 21.0d0 * x + 32.0d0
    end function F_Poly_Third_F

    double precision function DF_Poly_Third_F(x)
        double precision, intent(in) :: x
        DF_Poly_Third_F = 3.0d0 * x**2.0d0 + 12.0d0 * x + 21.0d0
    end function DF_Poly_Third_F
    
    double precision function F_xsin(x)
        double precision, intent(in) :: x
        F_xsin = sin(x) - 0.5d0 * x
    end function F_xsin

    double precision function DF_xsin(x)
        double precision, intent(in) :: x
        DF_xsin = cos(x) - 0.5d0
    end function DF_xsin

    double precision function F(x, y)
        double precision, intent(in) :: x, y
        F = x**3.0d0 - 3.0d0 * x * y**2.0d0 + 6.0d0 * x**2.0d0 - 6.0d0 * y**2.0d0 + 21.0d0 * x +32.0d0
    end function F
    
    double precision function Fx(x, y)
        double precision, intent(in) :: x, y
        Fx = 3.0d0 * x**2.0d0 - 3.0d0 * y**2.0d0 + 12.0d0 * x + 21.0d0
    end function Fx

    double precision function Fy(x, y)
        double precision, intent(in) :: x, y
        Fy = - 6.0d0 * x * y - 12.0d0 * y
    end function Fy

    double precision function G(x, y)
        double precision, intent(in) :: x, y
        G = 3.0d0 * x**2.0d0 * y - y**3.0d0 + 12.0d0 * x * y + 21.0d0 * y
    end function G

    double precision function Gx(x, y)
        double precision, intent(in) :: x, y
        Gx = 6.0d0 * x * y + 12.0d0 * y
    end function Gx

    double precision function Gy(x, y)
        double precision, intent(in) :: x, y
        Gy = 3.0d0 * x**2.0d0 - 3.0d0 * y**2.0d0 + 12.0d0 * x + 21.0d0
    end function Gy

    real(real64) function Binary_search(f, a, b, convergence, epsilon, maxIter)
        implicit none
        interface
            double precision function f(x)
                double precision, intent(in) :: x
            end function f
        end interface
        real(real64), intent(inout)   :: a, b
        real(real64), intent(in)   :: convergence, epsilon
        integer(int32), intent(in) :: maxIter
        real(real64)   :: fa, fb, fc
        real(real64)   :: c
        integer(int32) :: i

        fa = f(a)
        fb = f(b)

        if (fa * fb > 0.0d0) then
            write(*,*) "f(a) and f(b) have the same sign."
            stop
        end if

        do i = 1, maxIter
            c = (a + b) / 2.0d0
            fc = f(c)

            if (abs(fc) < convergence .and. (abs(a - b) < epsilon)) then
                Binary_search = c
                return
            end if

            if (fa * fc < 0.0d0) then
                b = c
                fb = fc
            else
                a = c
                fa = fc
            end if
        end do

        write(*,'(a)') "Warning: Convergence conditions not reached"
        Binary_search = c

    end function Binary_search

    real(real64) function Newton_Method_First(f, df, a, convergence, maxIter)
        implicit none
        interface
            double precision function f(x)
                double precision, intent(in) :: x
            end function f
            double precision function df(x)
                double precision, intent(in) :: x
            end function df
        end interface
        real(real64), intent(in)   :: a, convergence
        integer(int32), intent(in) :: maxIter
        real(real64)   :: c, fc, dfc
        integer(int32) :: i

        c = a

        do i = 1, maxIter
            fc = f(c)
            dfc = df(c)
            
            if (abs(dfc) < 1.0d-20) then
                write(*,'(a)')  "Warning: Convergence not possible due to derivatives close to zero"
                Newton_Method_First = c
                return
            end if
            
            c = c - fc / dfc
            
            if (abs(fc) < convergence) then
                Newton_Method_First = c
                return
            end if
        end do


        write(*,'(a)') "Warning: Convergence conditions not reached"
        Newton_Method_First = c
    end function Newton_Method_First

subroutine Newton_Method_2D(f, g, fx, fy, gx, gy, a, b, convergence, maxIter, x_star, y_star, status)
        implicit none
        interface
            double precision function f(x, y)
                double precision, intent(in) :: x, y
            end function f
            double precision function g(x, y)
                double precision, intent(in) :: x, y
            end function g
            double precision function fx(x, y)
                double precision, intent(in) :: x, y
            end function fx
            double precision function fy(x, y) 
                double precision, intent(in) :: x, y
            end function fy
            double precision function gx(x, y)
                double precision, intent(in) :: x, y
            end function gx
            double precision function gy(x, y)
                double precision, intent(in) :: x, y
            end function gy
        end interface
        real(real64), intent(in)   :: a, b, convergence
        integer(int32), intent(in) :: maxIter
        real(real64), intent(out)  :: x_star, y_star
        integer(int32), intent(out):: status
        real(real64)   :: x, y, fx_val, fy_val, gx_val, gy_val
        real(real64)   :: f_val, g_val, detJ
        integer(int32) :: i


        x = a
        y = b

        do i = 1, maxIter
            f_val = f(x, y)
            g_val = g(x, y)
            fx_val = fx(x, y)
            fy_val = fy(x, y)
            gx_val = gx(x, y)
            gy_val = gy(x, y)
            
            detJ = fx_val * gy_val - fy_val * gx_val
            
            if (abs(detJ) < 1.0d-20) then
                write(*, '(a)') "Warning: Convergence not possible due to Jacobian determinant close to zero"
                x_star = x
                y_star = y
                status = 1
                return
            end if
            

            x = x - ( gy_val * f_val - fy_val * g_val) / detJ
            y = y - (-gx_val * f_val + fx_val * g_val) / detJ

            if (sqrt(f_val**2 + g_val**2) < convergence) then
                x_star = x
                y_star = y
                status = 0
                return
            end if
        end do

        write(*, '(a)') "Warning: Convergence conditions not reached"
        x_star = x
        y_star = y
        status = 1
    end subroutine Newton_Method_2D

end program Lecture_04
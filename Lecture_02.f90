program Lecture_02
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32) :: num_cases

    write(*,*) "Enter the problem number:"
    read(*,*) num_cases

    select case(num_cases)
        case (1)
            write(*,*) "Taylor expansion"
        case (2)
            block
                integer(int32) :: i1, i2, i3
                integer(int32) :: ipiv(3), info
                real(real64)   :: x1, x2, x3
                real(real64)   :: y1, y2, y3
                real(real64)   :: x(3), y(3), Coe(3)
                real(real64)   :: a, b, c
                real(real64)   :: Am(3,3)
                real(real64)   :: det
                real(real64)   :: x_obj, y_obj
                
                i1 = 1
                i2 = 2
                i3 = 3

                x(:) = [1.1d0, 1.2d0, 1.3d0]
                y(:) = [0.89121d0, 0.03204d0, 0.96356d0]
            
                Am(1,:) = [1.0d0, x(1), x(1)**2.0d0]
                Am(2,:) = [1.0d0, x(2), x(2)**2.0d0]
                Am(3,:) = [1.0d0, x(3), x(3)**2.0d0]

                call Solver_LU(Am, y, x, 3)

                write(*,'(a, f16.11)'), "a (LU) = ", x(1)
                write(*,'(a, f16.11)'), "b (LU) = ", x(2)
                write(*,'(a, f16.11)'), "c (LU) = ", x(3)

                x(:) = [1.1d0, 1.2d0, 1.3d0]
                y(:) = [0.89121d0, 0.03204d0, 0.96356d0]
                Am(1,:) = [1.0d0, x(1), x(1)**2.0d0]
                Am(2,:) = [1.0d0, x(2), x(2)**2.0d0]
                Am(3,:) = [1.0d0, x(3), x(3)**2.0d0]

                det = (x(3) - x(1)) * (x(3) - x(2)) * (x(2) - x(1))
                a = ((Am(2,2)*Am(3,3) - Am(3,2)*Am(2,3)) * y(1) + (Am(3,2)*Am(1,3) - Am(1,2)*Am(3,3)) * y(2) + (Am(1,2)*Am(2,3) - Am(2,2)*Am(1,3)) * y(3)) / det
                b = ((Am(3,1)*Am(2,3) - Am(2,1)*Am(3,3)) * y(1) + (Am(1,1)*Am(3,3) - Am(3,1)*Am(1,3)) * y(2) + (Am(2,1)*Am(1,3) - Am(1,1)*Am(2,3)) * y(3)) / det
                c = ((Am(2,1)*Am(3,2) - Am(3,1)*Am(2,2)) * y(1) + (Am(3,1)*Am(1,2) - Am(1,1)*Am(3,2)) * y(2) + (Am(1,1)*Am(2,2) - Am(2,1)*Am(1,2)) * y(3)) / det
                
                write(*,'(a, f16.11)') "a      = ", a
                write(*,'(a, f16.11)') "b      = ", b
                write(*,'(a, f16.11)') "c      = ", c

                x_obj = 1.16
                y_obj = a + b*x_obj + c*x_obj**2.0d0

                write(*,'(a, f16.11)') "y_obj  = ", y_obj

            end block
        case (3)
            block
                real(real64) :: x(4), f(4)
                real(real64) :: result

                x(:) =  [9.0d0, 9.5d0, 10.0d0, 11.0d0]
                f(:) =  [2.19722d0, 2.25129d0, 2.30259d0, 2.39790d0]

                result = Lagrange(x, f, 4, 9.2d0)
                write(*,'(a, f14.12)') "L (9.2) = ", result
                write(*,'(a, f14.12)') "ln(9.2) = ", log(9.2d0)


            end block
        case (4)
            block
                write(*,*) "to be written"
            end block
        case (5)
            block
                integer(int32)            :: N
                integer(int32)            :: i,j,k, iv
                integer(int32)            :: vsize
                real(real64), allocatable :: x(:), f(:), x_Lag(:), f_Lag(:), norms(:), MSE(:)
                real(real64), allocatable :: analytic(:,:), Lagrange_result(:,:), Spline(:)
                real(real64), allocatable :: a(:), b(:), c(:), d(:)
                real(real64)              :: result
                character(len=100)        :: filename

                write(*,*) "Enter the number of division, N:"
                read(*,*) N
                write(*,*) "Enter Size(0:small, 1:medium, 2:big) "
                read(*,*) i
                if (i == 0) then
                    vsize = 12**4
                else if (i == 1) then
                    vsize = 12**5
                else if (i == 2) then
                    vsize = 12**6
                else
                    write(*,*) "Invalid size."
                    stop
                end if

                allocate(analytic(2,0:vsize))
                allocate(Lagrange_result(4,0:vsize))
                allocate(Spline(0:vsize))
                do i = 0, vsize
                    analytic(1,i) = -1.0d0 + 2.0d0 * dble(i) / dble(vsize)
                    analytic(2,i) =  1.0d0 / (1.0d0 + 25.0d0 * (analytic(1,i))**2.0d0)
                end do

                allocate(x(0:N))
                allocate(f(0:N))
                allocate(norms(5))
                allocate(MSE(5))

                do iv = 0, N
                    x(iv) = -1.0d0 + 2 * dble(iv) / dble(N)
                    f(iv) = 1.0d0 / (1.0d0 + 25.0d0 * x(iv)**2.0d0)
                end do

                ! Lagrange interpolation
                !$omp parallel do private(i, j, k, x_Lag, f_Lag)
                do j = 1, 4
                    Lagrange_result(j,:) = 0.0d0
                    if (.not. allocated(x_Lag)) allocate(x_Lag(0:j))
                    if (.not. allocated(f_Lag)) allocate(f_Lag(0:j))
                    do k = 1, N
                        do i = 0, j
                            x_Lag(i) = x(k-1) + (x(k) - x(k-1)) * (dble(i)/dble(j))
                            f_Lag(i) = 1.0d0 / (1.0d0 + 25.0d0 * (x_Lag(i))**2.0d0)
                        end do
                        do i = (k-1) *vsize/N, k*vsize/N-1
                            Lagrange_result(j,i) = Lagrange_General(x_Lag, f_Lag, j, analytic(1,i))
                        end do
                    end do
                    Lagrange_result(j,vsize) = analytic(2,vsize)
                    deallocate(x_Lag)
                    deallocate(f_Lag)
                end do
                !$omp paralell end do

                ! 3-order Spline interpolation
                call cubic_spline_coefficients(N+1, x, f, a, b, c, d)
                do i = 0, vsize
                    Spline(i) = cubic_spline_interpolate(N+1, x, a, b, c, d, analytic(1,i))
                end do

                write(filename, '(a,i0,a)') "result/L2/f_res_", N,".dat"
                open(unit=10, file=filename, status='replace')
                    write(10,"(a,2x,a,2x,a,2x,a,2x,a,2x,a,2x,a)") "x", "f", "L1", "L2", "L3", "L4", "S1"
                    do i = 0, vsize
                        write(10,"(7f20.15)") analytic(1,i), analytic(2,i), Lagrange_result(1,i), Lagrange_result(2,i), Lagrange_result(3,i), Lagrange_result(4,i), Spline(i)
                    end do
                close(10)

                do j = 1, 4
                    do iv = 0, vsize
                        norms(j) = norms(j) + (analytic(2, iv) - Lagrange_result(j, iv))**2.0d0
                    end do
                    MSE(j) = norms(j) / vsize
                end do
                do iv = 0, vsize
                    norms(5) = norms(5) + (analytic(2, iv) - Spline(iv))**2.0d0
                end do
                MSE(5) = norms(5) / vsize
                print '(a,i0)',       "Size = ", vsize
                print '(a, 5es15.8)', "norm = ", norms
                print '(a, 5es15.8)', "MSE  = ", MSE

                deallocate(x)
                deallocate(f)
                deallocate(analytic)
                deallocate(Lagrange_result)
                deallocate(norms)
                deallocate(MSE)
            end block

        case (:0)
            write(*,*) "Invalid problem number."
        case (6:)
            write(*,*) "Invalid problem number."
    end select

    contains

    function Lagrange(x, f, n, obs) result(result)
        implicit none
        integer(int32), intent(in)  :: n
        real(real64), intent(in)    :: x(:), f(:)
        real(real64), intent(in)    :: obs
        real(real64)                :: L, result
        integer(int32)              :: i, j

        result = 0.0d0
        do i = 1, n
            L = 1.0d0
            do j = 1, n
                if (j /= i) then
                    L = L * (obs - x(j)) / (x(i) - x(j))
                end if
            end do
            result = result + f(i) * L
        end do

    end function Lagrange

    function Lagrange_General(x, f, n, obs) result(result)
        implicit none
        integer(int32), intent(in)  :: n
        real(real64), intent(in)    :: x(0:), f(0:)
        real(real64), intent(in)    :: obs
        real(real64)                :: L, result
        integer(int32)              :: i, j

        result = 0.0d0
        do i = 0, n
            L = 1.0d0
            do j = 0, n
                if (j /= i) then
                    L = L * (obs - x(j)) / (x(i) - x(j))
                end if
            end do
            result = result + f(i) * L
        end do

    end function Lagrange_General

    subroutine cubic_spline_coefficients(n, x, y, a, b, c, d)
        integer(int32), intent(in)               :: n
        real(real64), intent(in)                 :: x(0:), y(0:)
        real(real64), allocatable, intent(inout) :: a(:), b(:), c(:), d(:)
        real(real64), allocatable                :: h(:), alpha(:), l(:), mu(:), z(:)
        integer(int32)                           :: i

        ! 配列の割り当て
        if (.not. allocated(a))     allocate(a(n))
        if (.not. allocated(b))     allocate(b(n-1))
        if (.not. allocated(c))     allocate(c(n))
        if (.not. allocated(d))     allocate(d(n-1))
        
        if (.not. allocated(h))     allocate(h(n-1))
        if (.not. allocated(alpha)) allocate(alpha(n-1))
        if (.not. allocated(l))     allocate(l(n))
        if (.not. allocated(mu))    allocate(mu(n))
        if (.not. allocated(z))     allocate(z(n))

        a(:)  = y(0:)
        c(:)  = 0.0d0
        l(1)  = 1.0d0
        mu(1) = 0.0d0
        z(1)  = 0.0d0

        do i = 1, n-1
            h(i) = x(i) - x(i-1)
        end do
        do i = 2, n-1
            alpha(i) = (3.0d0/h(i)) * (a(i+1) - a(i)) - (3.0d0/h(i-1)) * (a(i) - a(i-1))
        end do

        do i = 2, n-1
            l(i) = 2.0d0 * (x(i+1) - x(i-1)) - h(i-1) * mu(i-1)
            mu(i) = h(i) / l(i)
            z(i) = (alpha(i) - h(i-1) * z(i-1)) / l(i)
        end do
        l(n) = 1.0d0
        z(n) = 0.0d0
        c(n) = 0.0d0

        do i = n-1, 1, -1
            c(i) = z(i) - mu(i) * c(i+1)
            b(i) = (a(i+1) - a(i)) / h(i) - h(i) * (c(i+1) + 2.0d0 * c(i)) / 3.0d0
            d(i) = (c(i+1) - c(i)) / (3.0d0 * h(i))
        end do

        deallocate(h)
        deallocate(alpha)
        deallocate(l)
        deallocate(mu)
        deallocate(z)

    end subroutine cubic_spline_coefficients

    function cubic_spline_interpolate(n, x, a, b, c, d, xi) result(yi)
        integer(int32), intent(in) :: n
        real(real64), intent(in)   :: x(:), a(:), b(:), c(:), d(:), xi
        real(real64)               :: yi
        integer(int32)             :: i

        ! 補間区間の特定
        do i = 1, n-1
            if (xi >= x(i) .and. xi <= x(i+1)) then
                yi = a(i) + b(i) * (xi - x(i)) + c(i) * (xi - x(i))**2 + d(i) * (xi - x(i))**3
                return
            end if
        end do
        yi = 0.0d0  ! xiが範囲外の場合
    end function cubic_spline_interpolate

    subroutine Solver_LU(LU_A, LU_b, LU_x, N)
        implicit none
        real(real64),   intent(inout) :: LU_A(:,:), LU_b(:), LU_x(:)
        integer(int32), intent(in)    :: N
        integer(int32)                :: ipiv(N), info

        !* LU decomposition
        call Dgetrf(N, N, LU_A, N, ipiv, info)
        ! if (info /= 0) call error_message(942)

        !* solve linear equation
        call Dgetrs('N', N, 1, LU_A, N, ipiv, LU_b, N, info)
        ! if (info /= 0) call error_message(943)

        LU_x(:) = LU_b(:)

    end subroutine Solver_LU


end program Lecture_02
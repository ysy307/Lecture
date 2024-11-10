program Lecture_05
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32) :: num_cases
    real(real64)   :: A2(2,2), x2(2), b2(2)
    real(real64)   :: A3(3,3), x3(3), b3(3)
    real(real64)   :: A5(5,5), x5(5), b5(5)
    integer(int32),parameter :: maxIter = 10000

    A2 = transpose(reshape([5.0d0, 4.0d0, 2.0d0, 3.0d0], [2,2]))
    x2 = [0.0d0, 0.0d0]
    b2 = [13.0d0, 8.0d0]
    
    A3 = transpose(reshape([1.0d0, 2.0d0, 1.0d0, 3.0d0, 8.0d0, 7.0d0, 2.0d0, 7.0d0, 4.0d0], [3,3]))
    x3 = [0.0d0, 0.0d0, 0.0d0]
    b3 = [3.0d0, 5.0d0, 8.0d0]

    A5 = transpose(reshape([10.0d0, 3.0d0, 1.0d0, 2.0d0, 1.0d0, 1.0d0, 19.0d0, 2.0d0, -1.0d0, 5.0d0, -1.0d0, 1.0d0, 30.0d0, 1.0d0, 10.0d0, -2.0d0, 0.0d0, 1.0d0, 20.0d0, 5.0d0, -3.0d0, 5.0d0, 1.0d0, -2.0d0, 25.0d0], [5,5]))
    x5 = [0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0]
    b5 = [-22.0d0, 27.0d0, 89.0d0, -73.0d0, 22.0d0]

    write(*,*) "Enter the problem number:"
    read(*,*) num_cases

    select case(num_cases)
        case (1)
            block
                real(real64)   :: x(3), y(3)
                real(real64)   :: A(3,3)

                x(:) = [1.1d0, 1.2d0, 1.3d0]
                y(:) = [0.89121d0, 0.03204d0, 0.96356d0]
            
                A(1,:) = [1.0d0, x(1), x(1)**2.0d0]
                A(2,:) = [1.0d0, x(2), x(2)**2.0d0]
                A(3,:) = [1.0d0, x(3), x(3)**2.0d0]

                call Gaussian_Elimination(A, y, x, 3)

                write(*,'(a, f16.11)'), "x1 (GE) = ", x(1)
                write(*,'(a, f16.11)'), "x2 (GE) = ", x(2)
                write(*,'(a, f16.11)'), "x3 (GE) = ", x(3)

                x(:) = [1.1d0, 1.2d0, 1.3d0]
                y(:) = [0.89121d0, 0.03204d0, 0.96356d0]
                
                call LU(A, y, x, 3)
                
                write(*,'(a, f16.11)'), "x1 (LU) = ", x(1)
                write(*,'(a, f16.11)'), "x2 (LU) = ", x(2)
                write(*,'(a, f16.11)'), "x3 (LU) = ", x(3)


            end block
        case (2)
            block
                call Gaussian_Elimination(A3, b3, x3, 3)
                write(*,'(a,f13.10,a,f13.10,a,f13.10,a)'), '(x, y, z) = (', x3(1), ',', x3(2), ',', x3(3),')'
            end block
        case (3)
            block
                real(real64), parameter :: convergence = 1.0d-8

                call Jacobi_Method(A5, b5, x5, 5, convergence, maxIter)
                write(*,'(a)') "Jacobi Method:"
                write(*,'(a,f13.10,a,f13.10,a,f13.10,a,f13.10,a,f13.10,a)'), '(x1, x2, x3, x4, x5) = (', x5(1), ',', x5(2), ',', x5(3), ',', x5(4), ',', x5(5),')'
                write(*,'(a, es8.1)') "convergence delta : ", convergence

                x5(:) = [  0.0d0,  0.0d0,  0.0d0,   0.0d0,  0.0d0]
                b5(:) = [-22.0d0, 27.0d0, 89.0d0, -73.0d0, 22.0d0]
            
                call LU(A5, b5, x5, 5)
                write(*,'(a)') "LU Decomposition:"
                write(*,'(a,f13.10,a,f13.10,a,f13.10,a,f13.10,a,f13.10,a)'), '(x1, x2, x3, x4, x5) = (', x5(1), ',', x5(2), ',', x5(3), ',', x5(4), ',', x5(5),')'

            end block
        case (4)
            block
                real(real64), parameter :: convergence = 1.0d-8

                call Gauss_Seidel_Method(A5, b5, x5, 5, convergence, maxIter)
                write(*,'(a)') "Gauss Seidel Method:"
                write(*,'(a,f13.10,a,f13.10,a,f13.10,a,f13.10,a,f13.10,a)'), '(x1, x2, x3, x4, x5) = (', x5(1), ',', x5(2), ',', x5(3), ',', x5(4), ',', x5(5),')'
                write(*,'(a, es8.1)') "convergence delta : ", convergence

                x5(:) = [  0.0d0,  0.0d0,  0.0d0,   0.0d0,  0.0d0]
                b5(:) = [-22.0d0, 27.0d0, 89.0d0, -73.0d0, 22.0d0]
            
                call LU(A5, b5, x5, 5)
                write(*,'(a)') "LU Decomposition:"
                write(*,'(a,f13.10,a,f13.10,a,f13.10,a,f13.10,a,f13.10,a)'), '(x1, x2, x3, x4, x5) = (', x5(1), ',', x5(2), ',', x5(3), ',', x5(4), ',', x5(5),')'

            end block
        case (5)
            block
                real(real64), parameter :: convergence = 1.0d-18
                real(real64)            :: omega

                write(*,'(a)') "Enter the relaxation parameter omega:"
                read(*,*) omega
                if (omega < 1.0d0 .or. omega > 2.0d0) then
                    write(*,'(a)') "Error: omega must be between 1 and 2."
                    stop
                end if

                call SOR_Method(A2, b2, x2, 2, convergence, maxIter, omega)
                write(*,'(a)') "SOR Method:"
                write(*,'(a,f13.10,a,f13.10,a)'), '(x1, x2) = (', x2(1), ',', x2(2),')'
                write(*,'(a, es8.1)') "convergence delta : ", convergence

                x2(:) = [ 0.0d0, 0.0d0]
                b2(:) = [13.0d0, 8.0d0]

                call LU(A2, b2, x2, 2)
                write(*,'(a)') "LU Decomposition:"
                write(*,'(a,f13.10,a,f13.10,a)'), '(x1, x2) = (', x2(1), ',', x2(2),')'

            end block
        case(6)
            block
                real(real64), parameter :: convergence = 1.0d-18
                real(real64)            :: omega

                write(*,'(a)') "Enter the relaxation parameter omega:"
                read(*,*) omega
                if (omega < 1.0d0 .or. omega > 2.0d0) then
                    write(*,'(a)') "Error: omega must be between 1 and 2."
                    stop
                end if

                call SOR_Method(A2, b2, x2, 2, convergence, maxIter, omega)
                write(*,'(a)') "SOR Method:"
                write(*,'(a,f13.10,a,f13.10,a)'), '(x1, x2) = (', x2(1), ',', x2(2),')'
                write(*,'(a, es8.1)') "convergence delta : ", convergence

                x2(:) = [ 0.0d0, 0.0d0]
                b2(:) = [13.0d0, 8.0d0]

                call LU(A2, b2, x2, 2)
                write(*,'(a)') "LU Decomposition:"
                write(*,'(a,f13.10,a,f13.10,a)'), '(x1, x2) = (', x2(1), ',', x2(2),')'

            end block
        case (:0)
            write(*,*) "Invalid problem number."
        case (7:)
            write(*,*) "Invalid problem number."
    end select

contains

    subroutine LU(A, b, x, N)
        implicit none
        real(real64),   intent(inout) :: A(:,:), b(:), x(:)
        integer(int32), intent(in)    :: N
        integer(int32)                :: ipiv(N), info

        ! LU decomposition
        call Dgetrf(N, N, A, N, ipiv, info)

        ! solve linear equation
        call Dgetrs('N', N, 1, A, N, ipiv, b, N, info)

        x(:) = b(:)

    end subroutine LU
    
    subroutine Gaussian_Elimination(A, b, x, n)
        implicit none
        real(real64), intent(in)    :: A(:,:)
        real(real64), intent(in)    :: b(:)
        real(real64), intent(out)   :: x(:)
        integer(int32), intent(in)  :: n

        real(real64) :: temp
        real(real64), allocatable :: U(:,:), b_temp(:)
        integer :: i, j, k, max_row
        real(real64) :: factor, max_val

        allocate(U(n, n))
        allocate(b_temp(n))

        U = A
        b_temp = b

        do k = 1, n
            max_row = k
            max_val = abs(U(k, k))

            do i = k + 1, n
                if (abs(U(i, k)) > max_val) then
                    max_val = abs(U(i, k))
                    max_row = i
                end if
            end do

            if (max_val == 0.0d0) then
                print *, "The matrix is singular or nearly singular."
                deallocate(U, b_temp)
                return
            end if

            if (max_row /= k) then
                do j = 1, n
                    temp = U(k, j)
                    U(k, j) = U(max_row, j)
                    U(max_row, j) = temp
                end do
                temp = b_temp(k)
                b_temp(k) = b_temp(max_row)
                b_temp(max_row) = temp
            end if

            do i = k + 1, n
                factor = U(i, k) / U(k, k)
                U(i, k:n) = U(i, k:n) - factor * U(k, k:n)
                b_temp(i) = b_temp(i) - factor * b_temp(k)
            end do
        end do


        x = 0.0d0

        do i = n, 1, -1
            x(i) = (b_temp(i) - sum(U(i, i+1:n) * x(i+1:n))) / U(i, i)
        end do

        deallocate(U)
        deallocate(b_temp)

    end subroutine Gaussian_Elimination

    subroutine Jacobi_Method(A, b, x, n, tol, max_iter)
        implicit none
        real(real64), intent(in)    :: A(:,:)
        real(real64), intent(in)    :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(in)  :: n
        real(real64), intent(in)    :: tol
        integer(int32), intent(in)  :: max_iter

        real(real64), allocatable   :: x_new(:)
        real(real64)                :: error
        integer(int32)              :: i, j, k
        integer(int32)              :: nunit

        open(newunit=nunit, file="result/Jacobi_Error.dat", status="replace")
        write(nunit,'(2a)') "iterations", "error"

        allocate(x_new(n))
        
        if (all(abs(x) < epsilon(0.0d0))) x = 0.0d0

        do k = 1, max_iter
            error = 0.0d0
            
            do i = 1, n
                x_new(i) = b(i)
                do j = 1, n
                    if (j /= i) then
                        x_new(i) = x_new(i) - A(i, j) * x(j)
                    end if
                end do
                x_new(i) = x_new(i) / A(i, i)

                error = error + (x(i) - x_new(i))**2.0d0
            end do
            x(1:n) = x_new(1:n)
            
            error = sqrt(error)

            write(nunit,'(i0, es15.8)') k, error

            if (error < tol) then
                write(*,'(a,i0,a)') "Jacobi method converged after ", k, " iterations."
                exit
            end if
        end do

        if (error >= tol .and. k >= max_iter) then
            write(*,'(a,i0,a,es13.6)') "Jacobi method did not converge within the maximum number of iterations. Iteration = ", k - 1, ", Error = ", error
        end if
        close(nunit)

        deallocate(x_new)

    end subroutine Jacobi_Method

    subroutine Gauss_Seidel_Method(A, b, x, n, tol, max_iter)
        implicit none
        real(real64), intent(in)    :: A(:,:)
        real(real64), intent(in)    :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(in)  :: n
        real(real64), intent(in)    :: tol
        integer(int32), intent(in)  :: max_iter

        real(real64)                :: error, old_x
        integer(int32)              :: i, j, k
        integer(int32)              :: nunit


        if (all(abs(x) < epsilon(0.0d0))) x = 0.0d0

        open(newunit=nunit, file="result/GS_Error.dat", status="replace")
        write(nunit,'(2a)') "iterations", "error"

        do k = 1, max_iter
            error = 0.0d0
            
            do i = 1, n
                old_x = x(i)
                x(i) = b(i)
                
                do j = 1, n
                    if (j /= i) then
                        x(i) = x(i) - A(i, j) * x(j)
                    end if
                end do
                x(i) = x(i) / A(i, i)
                
                error = error + (x(i) - old_x)**2.0d0
            end do

            error = sqrt(error)

            if (error < tol) then
                write(*,'(a,i0,a)') "Gauss-Seidel method converged after ", k, " iterations."
                exit
            end if
        end do

        if (error >= tol .and. k >= max_iter) then
            write(*,'(a,i0,a,es13.6)')  "Gauss-Seidel method did not converge within the maximum number of iterations. Iteration = ", k - 1, ", Error = ", error
        end if
        close(nunit)

    end subroutine Gauss_Seidel_Method

    subroutine SOR_Method(A, b, x, n, tol, max_iter, omega)
        implicit none
        real(real64), intent(in)    :: A(:,:)
        real(real64), intent(in)    :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(in)  :: n
        real(real64), intent(in)    :: tol
        integer(int32), intent(in)  :: max_iter
        real(real64), intent(in)    :: omega

        real(real64)                :: error, old_xi
        integer(int32)              :: i, j, k
        integer(int32)              :: nunit

        if (omega < 1.0d0 .or. omega > 2.0d0) then
            write(*,'(a, f15.8)') "Error: omega must be between 1 and 2. Given omega = ", omega
            stop
        end if

        open(newunit=nunit, file="result/SOR_Error.dat", status="replace")
        write(nunit,'(2a)') "iterations", "error"

        do k = 1, max_iter
            error = 0.0d0

            do i = 1, n
                old_xi = x(i)
                x(i) = (1.0d0 - omega) * x(i) + omega * (b(i) - sum(A(i,1:i-1) * x(1:i-1)) - sum(A(i,i+1:n) * x(i+1:n))) / A(i, i)
                
                error = error + (x(i) - old_xi)**2.0d0
            end do

            error = sqrt(error)

            write(nunit,'(i0, es15.8)') k, error

            if (error < tol) then
                write(*,'(a,i0,a)') "SOR method converged after ", k, " iterations."
                exit
            end if
        end do

        if (error >= tol .and. k >= max_iter) then
            write(*,'(a,i0, a, es13.6)')  "SOR method did not converge within the maximum number of iterations. Iteration = ", k - 1, ", Error = ", error
        end if
        close(nunit)

    end subroutine SOR_Method

end program Lecture_05
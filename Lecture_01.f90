program Lecture_01
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32) :: num_cases

    write(*,*) "Enter the problem number:"
    read(*,*) num_cases

    select case(num_cases)
        case (1)
            block
                real(real32)  :: sx
                real(real32)  :: sresult
                real(real64)  :: dx
                real(real64)  :: dresult
                real(real128) :: qx
                real(real128) :: qresult

                sx = 0.00002311
                ! sresult = sqrt(1.0 - sx)  / (1.0 - sx) - sqrt(1.0 + sx) / (1.0 + sx)
                sresult = 1.0 / sqrt(1.0 - sx) - 1.0 / sqrt(1.0 + sx)
                ! sresult = ((1.0 + sx) * sqrt(1.0 - sx) - (1.0 - sx) * sqrt(1.0 + sx))/ (1.0 - sx**2)
                dx = 0.00002311d0
                dresult = 1.0 / sqrt(1.0 - dx) - 1.0 / sqrt(1.0 + dx)
                qx = 0.00002311q0
                qresult = 1.0q0 / sqrt(1.0q0 - qx) - 1.0q0 / sqrt(1.0q0 + qx)
                print*, "single   :", sresult
                print*, "double   :", dresult
                print*, "quad     :", qresult
                print*, "Diff(d-s):", real(dresult) - sresult
                print*, "Diff(q-d):", dble(qresult) - dresult
            end block
        case (2)
            block
                real(real32)  :: sx
                real(real32)  :: sresult1, sresult2
                real(real64)  :: dx
                real(real64)  :: dresult1, dresult2
                real(real128) :: qx
                real(real128) :: qresult1, qresult2

                sx = 0.003
                sresult1 = 1.0 / (sqrt(1.0-sin(sx)**2))-1.0
                sresult2 = sin(sx)**2 / ((1.0+cos(sx))*cos(sx))
                print*, "s1-single:", sresult1
                print*, "s2-single:", sresult2
                dx = 0.003d0
                dresult1 = 1.0 / (sqrt(1.0-sin(dx)**2))-1.0
                dresult2 = sin(dx)**2 / ((1.0+cos(dx))*cos(dx))
                print*, "s1-double:", dresult1
                print*, "s2-double:", dresult2
                qx = 0.003q0
                qresult1 = 1.0q0 / (sqrt(1.0q0-sin(qx)**2))-1.0q0
                qresult2 = sin(qx)**2 / ((1.0q0+cos(qx))*cos(qx))
                print*, "s1-quad  :", qresult1
                print*, "s2-quad  :", qresult2
            end block
        case (3)
            print *, "to be implemented"
        case (4)
            block
                integer(int32)            :: n, i, j, k, m
                integer(int32)            :: start, end
                real(real64), allocatable :: S_n(:,:)

                print *, "Enter the number of terms (n):"
                read(*, *) n
                if (n < 0 .or. n > 20) then
                    write(*,*) "Invalid number of terms."
                    stop
                end if

                allocate(S_n(0:2*n,0:n))
                S_n(:,:) = 0.0d0
                
                start = 0
                end = 2*n
                do i = start, end
                    S_n(i,0) = Seq_Sn(i)
                end do
                do j = 1, n
                    start = start + 1
                    end = end - 1
                    do k = start, end
                        S_n(k,j) = Aitken_Sn(S_n(k-1,j-1), S_n(k,j-1), S_n(k+1,j-1))
                    end do
                end do

                do i = 0, n
                    write(*,'(20(f20.17))') S_n(:n,i)
                end do

                deallocate(S_n)
            end block

        case (:0)
            write(*,*) "Invalid problem number."
        case (5:)
            write(*,*) "Invalid problem number."
    end select

    contains

    real(real64) function Seq_Sn(n)
        implicit none
        integer(int32), intent(in) :: n
        integer(int32) :: k

        Seq_Sn = 0.0d0
        do k = 0, n
            Seq_Sn = Seq_Sn + (-1.0d0)**k / (2.0d0 * k + 1.0d0)
        end do
    end function Seq_Sn

    real(real64) function Aitken_Sn(S1, S2, S3)
        implicit none
        real(real64) :: S1, S2, S3
        integer(int32) :: k

        Aitken_Sn = (S3 * S1 - S2**2) / (S3 - 2.0d0 * S2 + S1)
    end function Aitken_Sn


end program Lecture_01
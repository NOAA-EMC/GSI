       module weights

       save

       integer,allocatable :: kxid(:,:),kyjd(:,:)
       integer,allocatable :: kxi(:),kyj(:)

       real,allocatable :: wtswd(:,:),wtsed(:,:),wtnwd(:,:),
     *   wtned(:,:)
       real,allocatable :: wtsw(:), wtse(:), wtnw(:), wtne(:)

       real,allocatable :: rm1(:),rm2(:)

       end module weights

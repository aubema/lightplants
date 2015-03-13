c programme pour lire et calculer a partir des couleurs d'une image ppm ascii
c
c Declaration des variables
       integer i,j,nx,ny,k,n,nf,na,nt,ndf,nda,ndt,imageo(640,480)
       real image(640,480,3),scale,ratior,ratiob,ratioj,seuil,som
       real feuille(200,3)
       real tige(200,3)
       real autre(200,3),d,dmin
c initialisation des variables
       n=0
c scale est la taille d'un pixel en m
       scale=0.001
c programme principal
       open(unit=1,file='image.ppm',status='unknown')
        read(1,*) 
        read(1,*) nx,ny
        read(1,*)
        read(1,*) (((image(i,j,k),k=1,3),i=1,640),j=1,480)
       close(unit=1)
c lecture automatique des pixels
c (détection feuille)
       open(unit=2,file='feuille.txt',status='unknown')
       read(2,*) nf
       do i=1,nf
         read(2,*) feuille(i,1),feuille(i,2),feuille(i,3)
       enddo
      close(unit=2)
c (détection tige)
      open(unit=2,file='tige.txt',status='unknown')
       read(2,*) nt
       do i=1,nt
         read(2,*) tige(i,1),tige(i,2),tige(i,3)
       enddo
      close(unit=2)

c (détection autre)
      open(unit=2,file='autre.txt',status='unknown')
       read(2,*) na
       do i=1,na
         read(2,*) autre(i,1),autre(i,2),autre(i,3)
       enddo
      close(unit=2)

      seuil=220.
      open(unit=1,file='feuille.pgm',status='unknown')
      write(1,100)
      write(1,*) '640 480 3'
      ndf=0
      ndt=0
      nda=0
      do j=1,480
         do i=1,640


            dmin=10000000.
c imageo=1 => feuille imageo=2 => tige imageo=3 => autre
            do n=1,nf
              d=sqrt((image(i,j,1)-feuille(n,1))**2.+(image(i,j,2)-
     +        feuille(n,2))**2.+(image(i,j,3)-feuille(n,3))**2.)
              if (d.lt.dmin) then 
                 dmin=d
                 imageo(i,j)=1
              endif
            enddo
            do n=1,nt
              d=sqrt((image(i,j,1)-tige(n,1))**2.+(image(i,j,2)-
     +        tige(n,2))**2.+(image(i,j,3)-tige(n,3))**2.)
              if (d.lt.dmin) then 
                 dmin=d
                 imageo(i,j)=2
              endif
            enddo
            do n=1,na
              d=sqrt((image(i,j,1)-autre(n,1))**2.+(image(i,j,2)-
     +        autre(n,2))**2.+(image(i,j,3)-autre(n,3))**2.)
              if (d.lt.dmin) then 
                 dmin=d
                 imageo(i,j)=3
              endif                
            enddo
            write(2,*) imageo(i,j)
            if (imageo(i,j).eq.1) ndf=ndf+1
            if (imageo(i,j).eq.2) ndt=ndt+1
            if (imageo(i,j).eq.3) nda=nda+1
            write(1,*) imageo(i,j)
c            print*,dmin
         enddo
      enddo
      close(unit=1)
      print*,scale*scale*real(ndf),ndf,ndt,nda

 100  format('P2')

      stop
      end
  

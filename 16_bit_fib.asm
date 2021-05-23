;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KATAR YAZ ;;;;;;;;;;;;;;;;;;;;;;;;
KATAR_YAZ macro katar_offset     ; katar_offset katarin efektif adresi
    push dx
    push ax
    
    mov dx, offset katar_offset             
    mov ah, 09h
    int 21h
    
    pop ax
    pop dx
endm                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KATAR YAZ ;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; BASAMAK_HESAPLA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BASAMAK_HESAPLA macro liste_offset sayi_offset
    push ax             ;
    push bx             ;
    push cx             ;  Registerlerin onceki degerlerini koru
    push dx             ;
    
    
    xor cx, cx ; cx'i temizle
    xor dx, dx ; dx'i temizle
    xor ax, ax ; ax'i temizle
    xor bx, bx ; bx'i temizle
    
    mov si, offset liste_offset             
    mov di, offset sayi_offset
    
    mov cl, [si+1]
   
    hesapla:
        mov al, [si+2]
        
        cmp al, 48                          ; 0'a esit ya da buyuk mu ?            ;
        jl hatali_girdi                                                            ;
                                                                                   ;  amac karakterin rakam oldugunu kontrol etmek
        cmp al, 57                          ; 9'a esit ya da kucuk mu ?            ;
        jg hatali_girdi                                                            ;
        
        sub al, 48                          ; girilen karakteri rakama donustur ?  ;
                                                                                   ;
                                                                                   ;
        cmp cl, 0                           ; rakam sayisi 0 ise                   ;
        je sifira                                                                  ;
                                                                                   ; 
        cmp cl, 1                           ; rakam sayisi 1 ise                   ;
        je bire                                                                    ;
                                                                                   ;
        cmp cl, 2                           ; rakam sayisi 2 ise                   ;   Amac ilgili rakamin hanesini bulma
        je ikiye                                                                   ;
                                                                                   ;
        cmp cl, 3                           ; rakam sayisi 3 ise                   ;
        je uce                                                                     ;
                                                                                   ;
        cmp cl, 4                           ; rakam sayisi 4 ise                   ;
        je dorde                                                                   ;
                                                                                   ;
        cmp cl, 5                           ; rakam sayisi 5 ise                   ;
        je bese
        
        bese:                                                                      ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset ONBIN                                                    ;
           TOPLA                                                                   ;
        dorde:                                                                     ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset BIN                                                      ;
           TOPLA                                                                   ;
        uce:                                                                       ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset YUZ                                                      ;       Amac ilgili rakamin hanesiyle carpip girilen sayiyi bulmak
           TOPLA                                                                   ;
        ikiye:                                                                     ;
           xor bx, bx                                                              ;
           xor dx, dx                                                              ;
           mov bx, offset ON                                                       ;
           TOPLA                                                                   ;
                                                                                   ;
        bire:                                                                      ;
           add byte ptr[di], al                                                    ;
           jmp sifira                                                              ;
                                                                                   ;
        sifira:                                                                    ;
        xor ax, ax                                                                 ;
        xor dx, dx                                                                 ;
                                                                                   ;
        inc si                                                                     ;
    loop hesapla                                                                   ;
    
    pop dx        ;
    pop cx        ; 
    pop bx        ;  korunmus degerleri yigindan geri al
    pop ax        ;
                  
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TOPLA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Bu macro byte'a gore carpma ve toplama islemini gerceklestirmektedir;;;;;;;;;;;
TOPLA macro
    
   push ax                          ; ax'in degerini koru
   mul byte ptr [bx]                ; byte kadar carpma islemi yap
   add byte ptr[di], al             ; al yi sayinin ilk bayte'ina ekle
   adc byte ptr[di+1], ah           ; ah'daki degeri(byte * 4 bit = en fazla 2 byte(16 bit)) sayini ikinci bayte'ina carry ile beraber ekle.
   pop ax                           ; ax'in onceki degerini yigindan al.

   push ax
   mul byte ptr [bx+1]
   add byte ptr[di+1], al
   adc byte ptr[di+2], ah
   pop ax
   jmp sifira                       

endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TOPLA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BASAMAK_HESAPLA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
org 100h
     
    .data                                   ; Degiskenlerin tanimlandigi yer
        ON              dw 10               ;
        YUZ             dw 100              ;
        BIN             dw 1000             ;
        ONBIN           dw 10000            ;
        
        KULLANICI_TAM_SAYI_GIR_SORGUSU  db  'Lutfen bir sayi giriniz(Max: 4.100.200.300):','$'
        HATA     db 'Hatali girdi, !!!!!','$'; HATALI input ciktisi
        SAYI     dw ?                       ; kullanicinin girdigi sayi
        BUFF     db  6                      ; olabilecek max rakam (11).
                 db  ?                      ; kullanicinin girdigi rakamlarin sayini tutacak.
                 db  6 dup(0)               ; girilen karakterler. 
        FIB1 dw 1                           ; Fibonacci n-1 icin
        FIB0 dw 0                           ; Fibonacci n-2 icin
        SAYAC   dw 0                        ; Kac tane fibonacci sayisi var ?
        LPARA db 'F(','$'                   ; 
	    RPARA db '): ','$'                  ;  gerekli ciktilar
	    ASALLIK db ' (Asal)','$'            ;                                  
	    RAKAMLAR DB 10 DUP(0)               ;sayinin rakamlarini tutacak dizi
        N DW 0                              ; BASAMAK SAYISI        
        YEDI DB 1,3,2,-1,-3,-2,1,3,2,-1     ; yedi'nin bolunebilme kurali icin 
        INDIS DB 0                          ; Bolunebilme kurallari icin o anki basamagin cift mi tek mi 2'ye bolunerek ogremek icin kullanilacak degisken
    .code
        
        mov ax, @data
        mov ds, ax
        mov es, ax
        
        sayi_al:
            KATAR_YAZ KULLANICI_TAM_SAYI_GIR_SORGUSU            ; Kullanicidan girdi isteme
            call SATIR_ATLA                                     ; yeni satira gec
            mov ah, 0Ah                                         ; Kullanicidan string oku
            mov dx, offset BUFF                                 ; dx'e BUFF in efektif adresini ata
            int 21h;                                            ; Girdi icin interrupt olustur
            
            BASAMAK_HESAPLA BUFF SAYI                           ; Kullanicidan girdilerin integer karsiligini bul
            call SATIR_ATLA                                     ; yeni satira gec
            jmp fib                                             ; fibonacci sayilarini bulan fib etiketine dallan

        hatali_girdi:
            call SATIR_ATLA                                     ; yeni satira gec
            KATAR_YAZ HATA                                      ; hatayi yaz
            call SATIR_ATLA                                     ; yeni satira gec
            jmp sayi_al                                         ; tekrar kullanicidan sayi almak uzere sayi al etiketine dallan.
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    FIB:  
    mov ax, FIB1      ;
    cmp SAYI, ax      ; fibonacci < girilen sayidan daha kucuk olup olmadigini kontrol et
    jb FIB_SON        ;

    
    mov bx, FIB1      ;   TEMP = FIB1  bx = temp ; 
    mov ax, FIB0      ;                          ;    n - 1 fibonacciyi korumak icin bx'i gecici degisken olarak kullan
    add FIB1, ax      ;   FIB1 = FIB0            ;
    mov FIB0, bx      ;   FIB0 = TEMP            ;
    
    ;mov ax, offset LPARA                         ;
    ;call YAZDIR                                  ;
    KATAR_YAZ LPARA
    
    mov ax, SAYAC        ; SAYAC'i ax'e ata      ;
    call SAYI_YAZDIR            ; SAYAC'i yazdir                      ;  amac  ' F(SAYAB) :  Fibonacci ' yi yazdirmak
    
    
    ;mov ax, offset RPARA                         ;
    ;call YAZDIR                                  ; 
    KATAR_YAZ RPARA
    
    mov ax, FIB0         ; FIB0'i ax'e ata       ;
    call SAYI_YAZDIR            ; n'inci fibonacci sayisni  yazdir
    
    cmp FIB0,1           ;1 ise asal
    je ASAL              ; asal yazmak uzere asal etketine dallan
 
    cmp FIB0,2           ; 2 ISE ASAL
    je ASAL              ; asal yazmak uzere asal etketine dallan
    
    xor DX,DX     ; dx'i temizle
    mov ax, FIB0  ; fib0'i ax'e ata
    mov CX, 2     ;
    div CX        ;    2'YE BOLME
    cmp DX,0      ;    kalan 0 ise  
    je NOT_ASAL   ;    asal degil ve asal_degil etiketine dallan
      
      
       
    mov N,0                    ;
    mov ax, FIB0               ;
    xor SI,SI                  ;
    basamak:                   ;
        xor DX,DX              ;   amac n'inci fibonacci sayisinin rakamlarini bulmak 
        mov CX, 10             ;
        div CX                 ;       
        mov RAKAMLAR[SI], DL   ;       
        INC N                  ; basamak sayisini tutan degiskeni bir arttir
        INC SI                 ; adresi ilerlet
        cmp ax,0               ; son basamak mi sayi sifirdan buyuk ise
        JA basamak             ;
           
           
    
    cmp FIB0,3            ; fibonacci 3 ise esit ise
    je ASAL               ; asal etiketine dallan
                          ;
    xor SI,SI             ; si'yi temizle
    xor ax,ax             ;    
    mov CX, N             ; basamak sayisini tutan degiskeni cx'e ata ; amac basamaklarda gezinlelim ve toplamlarini bolunebilme kurallari icin kullanalim
basamak_topla:                     ;
    add AL, RAKAMLAR[SI]  ;
    INC SI                ; 
    LOOP basamak_topla    ;         
                          ;
    mov CH, 3             ;  3 icin  bolunebilme kurali
    div CH                ;
    cmp AH, 0             ;
    je NOT_ASAL           ;
    
    
    
    cmp FIB0,5            ; fibonacci 5'e esit ise
    je ASAL               ; asal etiketine dallan
                          ;
    cmp RAKAMLAR[0], 0    ; RAKAMLAR dizisi fibonacci sayinin tum basamak rakamlarini tuttuguna gore
    je NOT_ASAL           ;
    cmp RAKAMLAR[0], 5    ; 5 icin  bolunebilme kurali     
    je NOT_ASAL           ;
                                       
     
     
    cmp FIB0,7            ; fibonacci 7'ye esit ise
    je ASAL               ; asal etiketine dallan
                          ;
    xor ax,ax             ;
    xor bx,bx             ;
    xor SI,SI             ;
    xor DI,DI             ;     
	mov DI, offset YEDI   ; DIZI 
                          ;
    mov CX, N             ; N basamak kadar gezin
gezin:                    ;                       
    push CX               ; cx'i koru
    mov AL,RAKAMLAR[SI]   ; basamaktaki sayiyi al'e kopyala                           [SI]
    mov CL,YEDI[SI]       ; yedi dizisinden ilgili basamaga karsilik gelen carpani al [SI]
    IMUL CL               ; 
    add bx,ax             ;    7 icin  bolunebilme kurali
    INC SI                ;
    INC DI                ;  YEDI dizisindeki sonraki sayiyi elde etmek icin di'yi bir arttir
    pop CX                ;  cx'i geri al - dongunun N kadar calismasi icin push pop edildi.
    LOOP gezin            ;  
                          ;
    mov ax,bx             ;
    mov CH,7              ;   7 icin  bolunebilme kurali
    Idiv CH               ;
                          ;
    cmp AH, 0             ;
    je NOT_ASAL           ;
                            
                            
                            
    cmp FIB0,11           ; fibonacci 11'e esit ise
    je ASAL               ; asal etiketine dallan
     
    xor SI,SI             ;      
	mov CX, N             ;
for:                      ;
    push CX               ;
    xor AH,AH             ;         
    mov AL,INDIS          ;
    mov CH,2              ; INDIS -> amac -> n'inci basamagin cift mi tek mi diye anlamak +-+-...
    div CH                ;
    cmp AH,0              ; INDIS sifir ya da cift ise + pozitif olarak degerlendir
    je pozitif            ;
    sbb BL,RAKAMLAR[SI]   ; yoksa BL(pozitiflari tutan register)'den cikar 
    JMP sonraki           ;  dallan
                          ;
pozitif:                  ; 11'in  bolunme kurali
    add BL,RAKAMLAR[SI]   ;
                          ;
sonraki:                  ;
    INC SI                ;
    INC INDIS             ;
    pop CX                ;        
    LOOP for              ;
                          ;
    cmp bl, 0             ;
    jge devam             ;
    add bl, 11            ;
                          ;
    devam:                ;      
        mov ax,bx         ;             
        mov ch, 11        ;
        div ch            ;
        cmp ah, 0         ; 11'in  bolunme kurali
        je NOT_ASAL       ;
                                             
    
    
    cmp FIB0,13           ; fibonacci 13'e esit ise
    je ASAL               ;
    
    xor DX,DX             ;
    mov ax, FIB0          ; ax -> X
    mov CX, 10            ;
    div CX                ; X = 10a+b
    mov bx,ax             ; bx -> a
                          ; 13'nin  bolunme kurali
    mov ax,DX             ; ax -> b 
    mov CX,4              ;
                          ;
    MUL CX                ;   4*b
    add bx,ax             ;   a + 4*b
                          ;
    xor DX,DX             ;
    mov ax,bx             ;
    mov CX,13             ;
    div CX                ;
    cmp DX,0              ;
    je NOT_ASAL           ; 
    
    
    cmp FIB0,17           ; fibonacci 17'ye esit ise
    je ASAL               ; asal etiketine dallan
    
    xor DX,DX             ;
    mov ax, FIB0          ;   ax -> X
    mov CX, 10            ;
    div CX                ;   X = 10a+b
    mov bx,ax             ;   bx -> a
                          ;   17'nin  bolunme kurali
    mov ax,DX             ;   ax -> b 
    mov CX,5              ;
                          ;
    MUL CX                ;   5*b
    sub bx,ax             ;   a - 5*b
                          ;
    xor DX,DX             ;
    mov ax,bx             ;
    mov CX,17             ;
    div CX                ;
    cmp DX,0              ;
    je NOT_ASAL           ; 
    
    
    
    cmp FIB0,19           ; fibonacci 19'a esit ise
    je ASAL               ; asal etiketine dallan
           
    xor DX,DX       
    mov ax, FIB0          ;
    mov CX, 10            ;  ax -> X
    div CX                ;  X = 10a+b
    mov bx,ax             ;  bx -> a
                          ;  19'un  bolunme kurali
    mov ax,DX             ;  ax -> b 
    mov CX,2              ;  cx -> 2
    
    MUL CX                ;  2*b
    add bx,ax             ;  2*b + a
                          ;
    xor DX,DX             ;
    mov ax,bx             ;
    mov CX,19             ;  19'un  bolunme kurali
    div CX                ;
    cmp DX,0              ;
    je NOT_ASAL           ; 
    
    
    
    cmp FIB0,23           ;  fibonacci 23'e esit ise
    je ASAL               ;  asal etiketine dallan
           
    xor DX,DX             ;  23'un  bolunme kurali
    mov ax, FIB0          ;  ax -> X 
    mov CX, 10            ;  cx -> 10
    div CX                ;  X  = 10*a+b
    mov bx,ax             ;  bx -> a
                          ; 
    mov ax,DX             ;  ax -> b
    mov CX,7              ;  cx -> 7
    
    MUL CX                ;  7*b
    add bx,ax             ;  7*b + a
                          ;
    xor DX,DX             ;
    mov ax,bx             ;
    mov CX,23             ;  23'un  bolunme kurali
    div CX                ;
    cmp DX,0              ;
    je NOT_ASAL           ; 

    
    ASAL:
        ;mov ax, offset ASALLIK      ;  asal olmasi durumunda                ;
        KATAR_YAZ ASALLIK  
    
    NOT_ASAL:    
                                     ; asal degilse sayet
                                     ; satir basina gec
        call SATIR_ATLA
        
    INC SAYAC                       ; SAYAC'i bir attir
    JMP FIB
    

FIB_SON:
        
        
        
        
        
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                
              
       
ret

SATIR_ATLA  proc near
    push dx
    push ax
    
    mov dl, 0dh                                             ; \r
    mov ah, 02h
    int 21h
    
    mov dl, 0ah                                             ; \n
    mov ah, 02h
    int 21h
    
    pop ax                                                  ; stack oldugu icin tersten pop ediyoruz            
    pop dx
    ret
SATIR_ATLA endp

;SAYI OUTPUT VERME				
SAYI_YAZDIR  proc NEAR
        push cx              ; cx'i koru
        push dx              ; dx'i koru
        xor dx,dx            ; dx'i temizle
        push dx              ;
        mov cx,10            ;
        cmp ax,0             ; 
        jge digit_al         ;
        
digit_al:				     ;
        div cx               ;
        add dx,'0'           ;
        push dx              ;
        xor dx,dx            ; amac -> digitleri bul ve yigina at
        cmp ax,0             ;
        jne digit_al         ; 
        
goster:                      ;
        pop ax               ;
        cmp ax,0             ;
        je gbitir            ;
        call KARAKTER_YAZDIR ; amac -> yigindan karakter oku ve bastir           
        JMP goster           ;
        
gbitir:                      ;
        pop dx               ;
        pop cx               ;  korunmus degerleri yigindan geri al
        RET                  ;
SAYI_YAZDIR    endp                 



;KARAKTER OUTPUT VERME
KARAKTER_YAZDIR proc NEAR            
		push ax              ;
		push dx              ;
		mov DL,AL            ; KAYNAK: emu8086
		mov AH,2             ; INT 21h / AH=2 - write character to standard output.
		int 21h              ; DL = character to write, after execution AL = DL.
		pop dx               ;
		pop ax               ;
		ret                  ;
KARAKTER_YAZDIR endp
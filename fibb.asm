

KATAR_YAZ macro katar_offset                ; katar_offset katarin efektif adresi
    mov dx, offset katar_offset
    mov ah, 09h
    int 21h
endm                                

;;;;;;;;;;;;;;;;;;  BASAMAK HESAPLA ;;;;;;;;;;;;;;;;;;;;
BASAMAK_HESAPLA macro liste_offset sayi_offset  
    
    xor cx, cx ; cx'i temizle
    xor dx, dx 
    xor ax, ax
    xor bx, bx 
    
    mov si, offset liste_offset
    mov di, offset sayi_offset
    ;mov bx, 10 ;offset basamak_offset
    
    mov cl, [si+1]
   
    hesapla:
        mov al, [si+2]
        
        cmp al, 48
        jl hatali_girdi
        
        cmp al, 57
        jg hatali_girdi
        
        sub al, 48

        ;push cx
        ;xlar:
            ;xor dx, dx
            ;cmp cl, 1
            ;je birler
            
            ;sub cl, 1
            ;lar:
                ;mul bx
            ;loop lar
            ;birler:
                ;add word ptr [di], al
        ;pop cx
               
        cmp cl, 0 ; 
        je sifira
        
        cmp cl, 1 ; 
        je bire
        
        cmp cl, 2 ; 
        je ikiye
        
        cmp cl, 3 ; 
        je uce
        
        cmp cl, 4 ; 
        je dorde
        
        cmp cl, 5 ; 
        je bese
        
        cmp cl, 6 ; 
        je altiya
        
        cmp cl, 7 ; 
        je yediye
        
        cmp cl, 8 ; 
        je sekize
        
        cmp cl, 9 ; rakam sayisi 9 ise 
        je dokuza
        
        cmp cl, 10 ; rakam sayisi 10 ise
        je ona
        
        
        ona:
        
           xor bx, bx
           xor dx, dx
           mov bx, offset MILYAR
           ;mov di, offset TEMP
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
        
        dokuza:
           xor bx, bx
           xor dx, dx
           mov bx, offset YUZMIL
           ;mov di, offset TEMP
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              
        sekize:
           xor bx, bx
           xor dx, dx
           mov bx, offset ONMIL
           ;mov di, offset TEMP
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        yediye:
           xor bx, bx
           xor dx, dx
           mov bx, offset MIL
           ;mov di, offset TEMP
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;pop di
        altiya:
           xor bx, bx
           xor dx, dx
           mov bx, offset YUZBIN
           ;mov di, offset TEMP
           
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;pop di
        bese:
           xor bx, bx
           xor dx, dx
           mov bx, offset ONBIN
           ;mov di, offset TEMP
           
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;pop di
        dorde:
           ;mov al, [si+2]
           ;push di
           ;xor di, di
           xor bx, bx
           xor dx, dx
           mov bx, offset BIN
           ;mov di, offset TEMP
           
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           jmp sifira
           ;pop di
        uce:
           ;push di
           ;xor di, di
           xor bx, bx
           xor dx, dx
           mov bx, offset YUZ
           ;mov di, offset TEMP
           
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           push ax
           mul byte ptr [bx]
           add byte ptr[di], al
           adc byte ptr[di+1], ah
           pop ax
           
           push ax
           mul byte ptr [bx+1]
           add byte ptr[di+1], al
           adc byte ptr[di+2], ah
           pop ax
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;pop di
        ikiye:
           ;push di
           ;xor di, di
           xor bx, bx
           xor dx, dx
           mov bx, offset ON
           ;mov di, offset TEMP
           
           ;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
           
           push ax
           mul byte ptr [bx+2]
           add byte ptr[di+2], al
           adc byte ptr[di+3], ah
           pop ax
           
           push ax
           mul byte ptr [bx+3]
           add byte ptr[di+3], al
           adc byte ptr[di+3], ah
           pop ax
           jmp sifira
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        bire:
           add byte ptr[di], al
           jmp sifira
         
        sifira:
        
        xor ax, ax
        xor dx, dx
        
        inc si
    loop hesapla
endm

;;;;;;;;;;;;;;;;;;;;;; BASAMAK_HESAPLA ;;;;;;;;;;;;;;;;;;               
org 100h

    .data
        ON              dd 10               ;
        YUZ             dd 100              ;
        BIN             dd 1000             ;
        ONBIN           dd 10000            ;
        YUZBIN          dd 100000           ;      BASAMAKLAR 
        MIL             dd 1000000          ;
        ONMIL           dd 10000000         ;
        YUZMIL          dd 100000000        ;
        MILYAR          dd 1000000000       ;
            
        KULLANICI_TAM_SAYI_GIR_SORGUSU  db  'Lutfen bir sayi giriniz(Max: 4.100.200.300):','$'
        HATALI   db 'HATALI GIRDI', '$'     ; HATALI input ciktisi
        ;RANGE    DD F464176C               ; olabilecek max sayi
        SAYI     dd ?                       ; kullanicinin girdigi sayi
        BUFF    db  11                      ; olabilecek max rakam (11).
                db  ?                       ; kullanicinin girdigi rakamlarin sayini tutacak.
                db  11 dup(0)               ; girilen karakterler. 
        TEMP dd ?                           ; carpimin sunucunu tutacak temp
    .code
        
        mov ax, @data
        mov ds, ax
        mov es, ax
        
        
        KATAR_YAZ KULLANICI_TAM_SAYI_GIR_SORGUSU            ; Kullanicidan girdi isteme
        mov ah, 0Ah                                         ; Kullanicidan string oku
        mov dx, offset BUFF                                 ; DX'e BUFF in efektif adresini ata
        int 21h;                                            ; Girdi icin interrupt olustur
        
        BASAMAK_HESAPLA BUFF SAYI                           ; Kullanicidan girdilerin integer karsiligini bul
        
        hatali_girdi:
            ;call SATIR_ATLA
            ;KATAR_YAZ HATALI
        
        
        
       
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





KATAR_YAZ macro katar_offset                ; katar_offset katarin efektif adresi
    mov dx, offset katar_offset
    mov ah, 09h
    int 21h
endm                                
                    
BASAMAK_HESAPLA macro liste_offset sayi_offset basamak_offset
    push cx                                 ; cx'in onceki degerini koru
    push dx
    mov cx, 0                               ; cx i temizle
    ;push bx                                ; bx degerini koru yigina atarak
    mov bx, offset sayi_offset              ; kullanicidan gelen sayinin adrsini bx 'e ata
    mov si, offset liste_offset             ; girilen rakamlar dizisi
    mov di, offset basamak_offset           ; her bir rakama karsilik gelen basamak sayi
    mov cl, [si+1]                          ; kullanicinin kac basamak girdigini cx e ata
    ;mov si, [si+2]                          ; ilk karakterin adresi 
    
    hesapla:
        mov dx, 0                           ; dx'i temizle
        mov ah, 0                           ; clear ah
        mov al, [si+2]                      ; si(karakterleri iceren dizi) nin gosterdigi bellekteki degeri al'ye ata
        
        cmp al, 48                          ; karakter kontrolu ( rakam >= 48 )
        jl hatali_girdi                     ; ascii al - 48 dan kucukse hatali input olarak degerlendir
        
        cmp al, 57                          ; karakter kontrolu (rakam <= 57 )
        jg hatali_girdi                     ; ascii al - 57 dan buyukse hatali input olarak degerlendir
        
        sub al, 48                          ; sayiyiya donustur
        ;mov ah, 0                           ; clear ah
        push cx                             ; ilk loop cx'in degerini koru
        XLAR                                ; 
        pop cx
        add WORD ptr [bx+2], dx
        add  [bx], ax                              ; ilk loop cx'in degerini yigindan geri al
        ;add WORD ptr [bx+2], dx
        ;add ax, dx
        ;adc [bx], ax
        mov ax, 0
        mov dx, 0
        inc si                              ; Bir sonraki rakama gec
    loop hesapla
    pop dx
    pop cx
endm

XLAR macro
    ;push ax
    ;push dx
    cmp cl, 1                               ; Eger son rakam ise yahut kullanici 1 haneli sayi girdi ise 
    je birler
    
    sub cl, 1  
    lar:
        MUL [di]                              ; her defasinda ax'i 0Ah ile carp sonucuna dx:ax'e ata
        
    loop lar
    ;add [bx], ax
    ;add [bx], dx    
    ;pop dx
    ;pop ax
    birler:
       ;add [bx], ax
endm
                                                         
org 100h

    .data
        ON db 0Ah;
        ;BASAMAK dd 000000001b      
        ;YENI_SATIR    db  0DH , 0AH ,'$'    ; YENI SATIR ICIN
        KULLANICI_TAM_SAYI_GIR_SORGUSU  db  'Lutfen bir sayi giriniz(Max: 4.100.200.300):','$'
        ;BASARILI db 'BASARILI GIRDI', '$'   ; BASARILI input ciktisi
        HATALI   db 'HATALI GIRDI', '$'    ; HATALI input ciktisi
        ;RANGE    DD F464176C               ; olabilecek max sayi
        SAYI     dd ?                       ; kullanicinin girdigi sayi
        RAKAMLAR db 0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h
        BUFF    db  11                      ; MAX NUMBER OF CHARACTERS ALLOWED (11).
                db  ?                       ; NUMBER OF CHARACTERS ENTERED BY USER.
                db  11 dup(0)               ; CHARACTERS ENTERED BY USER.  
    .code
        
        KATAR_YAZ KULLANICI_TAM_SAYI_GIR_SORGUSU            ; Kullanicidan girdi isteme
        call SATIR_ATLA 
        mov ah, 0Ah                                         ; Kullanicidan string oku
        mov dx, offset BUFF                                 ; DX'e BUFF in efektif adresini ata
        int 21h;                                            ; Girdi icin interrupt olustur
        
        BASAMAK_HESAPLA BUFF SAYI ON                   ; Kullanicidan girdilerin integer karsiligini bul
        
        ;basarili_girdi:
            ;KATAR_YAZ BASARILI
        hatali_girdi:
            call SATIR_ATLA
            KATAR_YAZ HATALI
       
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

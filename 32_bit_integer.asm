;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KATAR YAZ ;;;;;;;;;;;;;;;;;;;;;;;;
KATAR_YAZ macro katar_offset                ; katar_offset katarin efektif adresi
    mov dx, offset katar_offset
    mov ah, 09h
    int 21h
endm                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KATAR YAZ ;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;; BASAMAK_HESAPLA ;;;;;;;;;;;;;;;;;;;;;;;;;;
;         Bu makro kullanici tarafindan girilen
;         girdinin decimal karsiligini bulup parametre
;         olarak aldigi degiskene atama yapar.
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BASAMAK_HESAPLA macro liste_offset sayi_offset  
    
    push ax             ;
    push bx             ;
    push cx             ;  Registerlerin onceki degerlerini koru
    push dx             ;
                        ;
    
    xor cx, cx ; cx'i temizle
    xor dx, dx ; dx'i temizle
    xor ax, ax ; ax'i temizle
    xor bx, bx ; bx'i temizle
    
    mov si, offset liste_offset              ; karakterleri tutan dizinin efektik adresini si'ye yukle 
    mov di, offset sayi_offset               ; kullanicinin girdigi sayiyi tutacak olan degiskenin efektif adresini di'ye yukle
    
    mov cl, [si+1]                           ; kullanicin kac karakter girdigini tutan degiskenin degerini cl'ye kopyala
   
    hesapla:
        mov al, [si+2]                       ;  hesapla etiketine her girdiginde karakterleri birer birer al'ye yukle
        
        
        cmp al, 48                          ; 0'a esit ya da buyuk mu ?
        jl hatali_girdi
        
        cmp al, 57                          ; 9'a esit ya da kucuk mu ? 
        jg hatali_girdi
        
        sub al, 48                          ; girilen karakteri rakama donustur ?

               
        cmp cl, 0                           ; rakam sayisi 0 ise
        je sifira
        
        cmp cl, 1                           ; rakam sayisi 1 ise
        je bire
        
        cmp cl, 2                           ; rakam sayisi 2 ise
        je ikiye
        
        cmp cl, 3                           ; rakam sayisi 3 ise
        je uce
        
        cmp cl, 4                           ; rakam sayisi 4 ise
        je dorde
        
        cmp cl, 5                           ; rakam sayisi 5 ise 
        je bese
        
        cmp cl, 6                           ; rakam sayisi 6 ise 
        je altiya
        
        cmp cl, 7                           ; rakam sayisi 7 ise 
        je yediye
        
        cmp cl, 8                           ; rakam sayisi 8 ise
        je sekize
        
        cmp cl, 9                           ; rakam sayisi 9 ise 
        je dokuza
        
        cmp cl, 10                          ; rakam sayisi 10 ise
        je ona
        
        
        ona:
           xor bx, bx
           xor dx, dx
           mov bx, offset MILYAR
           TOPLA
        
        dokuza:
           xor bx, bx
           xor dx, dx
           mov bx, offset YUZMIL
           TOPLA 
           
        sekize:
           xor bx, bx
           xor dx, dx
           mov bx, offset ONMIL
           TOPLA
           
        yediye:
           xor bx, bx
           xor dx, dx
           mov bx, offset MIL
           TOPLA
           
        altiya:
           xor bx, bx
           xor dx, dx
           mov bx, offset YUZBIN
           TOPLA
           
        bese:
           xor bx, bx
           xor dx, dx
           mov bx, offset ONBIN
           TOPLA
           
        dorde:
           xor bx, bx
           xor dx, dx
           mov bx, offset BIN
           TOPLA
           
        uce:
           xor bx, bx
           xor dx, dx
           mov bx, offset YUZ
           TOPLA
           
        ikiye:
           xor bx, bx
           xor dx, dx
           mov bx, offset ON
           TOPLA 
           
        bire:
           add byte ptr[di], al
           jmp sifira
         
        sifira:
        
        xor ax, ax
        xor dx, dx
        inc si
    loop hesapla
    
    pop dx
    pop cx
    pop bx
    pop ax
endm
;;;;;;;;;;;;;;;;;;;; BASAMAK_HESAPLA ;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TOPLA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Bu macro 32 bitlik 2 degiskeni byte'a gore carpma ve toplama islemini gerceklestirmektedir;;;;
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
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TOPLA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIBB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FIB_HESAPLA macro

    push ax             ;
    push bx             ;
    push cx             ;  Registerlerin onceki degerlerini koru
    push dx             ;
    
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    lea si,  SAYI       ;girilen sayiyi al
    lea di,  FIBS       ; n
    lea bx,  FIBN       ; n - 1
     
    
    fib_bul:
        
        mov ax , word ptr [di]
        cmp word ptr [si] ,  ax   ; birinci word u kontrol et
        mov  ax, word ptr [di+2]
        je ikinci_word
        ikinci_word:       
        
            cmp [si+2], ax
            je fib_son  
        
        xor ax, ax               ; ax i sil
        cmp byte ptr si, 0        ;
        je  sifir              ; sifira esit ise dallan
    
        cmp byte ptr si, 1
        je bir               ; bire esit ise dallan 
         
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
        push ax            
        push dx            
        
        mov ax, word ptr [bx] ; n - 2 
        add ax , word ptr[di] ; n - 1
        mov dx, word ptr[di] ; di'in ilk kismini koru
        
        mov word ptr[di], ax ; di'nin ilk kismini guncelle
        
        mov ax, word ptr[bx+2]
        adc ax, word ptr[di+2]
        
        mov word ptr[bx], dx    ; bx'in ilk word kismina diger sayinin korunmus ilk kismini ata
        mov dx, word ptr[di+2]  ; di'in ilk kismini koru, neden? onceki deger icin  
        
        mov word ptr[di+2], ax
        mov word ptr[bx+2], dx  
        
        FIB_YAZDIR

        pop dx
        pop ax
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp fib_bul                  ; tekrarla girilen sayiya kadar
    
    sifir:
       mov [bx], 1               ; n - 1
       ARTTIR                    ; SAYACI bir arttir
       ;EKLE                      ; Diziye ekle n'inci fibonacci sayisini
       jmp fib_son
    bir:
      mov [di], 1                ; n 
      ARTTIR
      jmp fib_son  
       
    
    fib_son:


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    pop dx
    pop cx
    pop bx
    pop ax
    
endm 

;;;;;;;;; ARTTIR ;;;;;;;;;;;;;;;;;; 
;;; Her cagirildiginda sayici bir arttirir ;;;;;;
ARTTIR macro 
    push si
    mov si, offset SAYAC   ; SAYACIN adreini si'ye ata
    inc si                 ; SAYACI bir arttir
    pop si
endm 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;; EKEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Her cagirildiginda FIBS dizisine ilgili fibonacci sayisini ekler;;;;;;;;;;
;EKLE macro
     ;push ax                    
                                ; bx -> n'inci fibonacci sayisini tutan bellek adresi
     ;mov ax, word ptr[bx]       ; n'inci saynin birinci word kismini ax'e ata     NEDEN ? cunku bellek bellek icin mov komutu calimiyor. 
     ;mov di, offset FIBS       ; fibonacci sayilarini tutacak olan dizi
     ;mov word ptr[di], ax       ; sayinin ilk word kadar kismi
     ;add di, 2
     ;mov ax , word ptr [bx+2]   ; n'inci saynin ikinci word kismini ax'e ata 
     ;mov word ptr[di], ax       ; sayinin ikinci word kadar kismi
     ;add di, 2                  ; 32 bit yani 4 byte kadar indisi arttir
  
     ;pop ax
    
;endm 


;;;;;;;;;;;;;;; FIB_YAZDIR ;;;;;;;;;;;;;;;;;;;;;;;;;

FIB_YAZDIR macro
    
    push ax
    push bx
    push dx
    
    xor ax,ax
    xor dx, dx      

    pop bx
    pop ax 

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
        HATA   db 'HATALI GIRDI', '$'       ; HATALI input ciktisi
        ;RANGE    DD F464176C               ; olabilecek max sayi
        SAYI     dd ?                       ; kullanicinin girdigi sayi
        BUFF    db  14                      ; olabilecek max rakam (11).
                db  ?                       ; kullanicinin girdigi rakamlarin sayini tutacak.
                db  14 dup(0)               ; girilen karakterler. 
        ;FIB1 db 1                           ; Fibonacci 1 icin
        ;FIB0 db 1                           ; Fibonacci 0 icin
        SAYAC db 0                          ; Kac tane fibonacci sayisi var ?
        FIBN dd  1
             ;dw 0                           ; n'inci fibonacci sayisini tutar
        FIBS dd  1                   ; Fibonacci sayilarini tutan dizi son degisken olarak tanimla her zaman.
        LPARA db 'F(','$'                                                  ; GEREKLI CIKTILAR
	    RPARA db '): ','$'                                                 ;
	    ASALLIK db ' (Asal)','$'   
        
    .code                                   ; kod segmenti
        
        mov ax, @data                       ; data segmentini ax'e yukle
        mov ds, ax
        mov es, ax
        
        
        sayi_al:                                                ; kullanicidan girdi alamaya yarayan etiket
            KATAR_YAZ KULLANICI_TAM_SAYI_GIR_SORGUSU            ; Kullanicidan girdi isteme
            call SATIR_ATLA                                     ; yeni satira gec
            mov ah, 0Ah                                         ; Kullanicidan string oku -> kaynak emu8086:;input of a string to DS:DX, fist byte is buffer size, second byte is number of chars actually read. this function does not add '$' in the end of string.
            mov dx, offset BUFF                                 ; dx'e BUFF in efektif adresini ata
            int 21h;                                            ; Girdi icin interrupt olustur
        
        BASAMAK_HESAPLA BUFF SAYI                               ; Kullanicidan girdilerin integer karsiligini bul
        jmp bitir                                               ; bitirmek uzere dallan
        
        
        hatali_girdi:
            call SATIR_ATLA                                     ; yeni satira gec
            KATAR_YAZ HATA                                      ; hatayi yaz
            call SATIR_ATLA                                     ; yeni satira gec
            jmp sayi_al                                         ; tekrar kullanicidan sayi almak uzere sayi al etiketine dallan.
            
        bitir:
ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SATIR_ATLA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Yeni satira gecmeye yarayan bir fonksiyon ;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SATIR_ATLA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
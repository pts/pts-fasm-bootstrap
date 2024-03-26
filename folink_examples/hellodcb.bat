tasm /t hellodc.tas
copy /b folink2.tas f.u00
tasm /t /q f.u00 folink2t.com
folink2t.com hellodc.obj hellodc.com

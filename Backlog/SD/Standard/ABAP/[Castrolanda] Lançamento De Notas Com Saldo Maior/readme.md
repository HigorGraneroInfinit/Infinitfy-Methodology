
Este desenvolvimento teve a finalidade de aumentar o limite de crédito nas transação VA01 e VA02

O processo é chamado no Report Standard:
MV45AFZZ.abap

Logo após é chamado o Include Z:
ZSDI_0121.abap

O Include Z chama a classe zsdcl_ret_conserto.abap que possui os seguintes métodos:

![image](https://user-images.githubusercontent.com/121947587/217535284-49bde9a7-7875-4580-969c-2a01fbd40c42.png)

Método ZSDCL_RET_CONSERTO~GET_QUANT_FATURADA:

![image](https://user-images.githubusercontent.com/121947587/217536349-60100b78-8fc1-458c-9556-83b2f95a8bb0.png)

Método ZSDCL_RET_CONSERTO~GET_QUANT_DISP

![image](https://user-images.githubusercontent.com/121947587/217536407-6b8a3f58-5fdb-4c4f-9b5f-3587dfc6129c.png)

Método ZSDCL_RET_CONSERTO~GET_DADOS
![image](https://user-images.githubusercontent.com/121947587/217536562-a92e60ca-4096-44aa-a2ac-76247b9766be.png)


O cenário executado foi na VA01:
![image](https://user-images.githubusercontent.com/121947587/217537186-d1d6324d-a284-415c-a18b-155721d4eb37.png)




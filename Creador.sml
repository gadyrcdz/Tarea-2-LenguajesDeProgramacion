(*Libreria para ayudarnos a abrir archivos, principalmente csv*)
open TextIO;
(*Libreria para manejar errores con los path*)
open OS.Path;

structure Creador = struct

    (* Función para solicitar rutas por medio de consola *)
    fun outPutM(msg: string) =
    let 
        val _ = TextIO.output(TextIO.stdOut, msg);
        val _ = TextIO.flushOut TextIO.stdOut; (* Forza hacer el print *)
        val input = TextIO.inputLine TextIO.stdIn;
    in
        input
    end;


    (* Función para convertir una cadena a mayúsculas *)
    fun toUpperCase str =
    let
        (* Función auxiliar para convertir un carácter a mayúsculas *)
        fun toUpperChar c = 
            if Char.isLower c then 
                Char.chr (Char.ord c - 32)
            else
                c
    in
        String.map toUpperChar str
    end

    (* Función que recibe una ruta de archivo, lo abre y guarda en él todos los 
   demás datos pedidos en el formato especificado *)
    fun saveData () = 
    let 
        (* Procesamos el valor que regresa outPutM, asegurándonos de manejar el caso NONE *)
        val input = case outPutM("\nIngresa la ruta de tu archivo: ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""

        val cuentaO = case outPutM("\nNúmero de cuenta origen: ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""

        val fecha = case outPutM("\nFecha y hora (YYYY-MM-DD HH:MM:SS): ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""

        val tipo = case outPutM("\nTipo de transacción (DEPOSITO, RETIRO, TRANSFERENCIA): ") of
            SOME s => toUpperCase (String.substring(s, 0, size s - 1))
          | NONE => ""

        val cuenta_destino = 
            if tipo = "TRANSFERENCIA" then
                case outPutM("\nNúmero de cuenta destino:  ") of
                    SOME s => String.substring(s, 0, size s - 1)
                  | NONE => ""
            else ""
            
        val monto = case outPutM("\nMonto:  ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""

        (* Abre el archivo para agregar los datos *)
        val file = TextIO.openAppend input;
    
    in
        TextIO.output(file, cuentaO ^ "," ^ fecha ^ "," ^ tipo ^ "," ^ monto ^ "," ^ cuenta_destino ^ "\n");
        TextIO.closeOut file
    end
    handle OS.SysErr _ => (print "El archivo no existe en la ruta proporcionada.\n");

    (* Función que elimina todos los datos de un archivo, dejándolo en blanco *)
    fun emptyFile() = 
    let 
        val input = case outPutM("\nIngresa la ruta de tu archivo: ") of
            SOME s => String.substring(s, 0, size s -1)
          | NONE => ""

        val fileOpen = TextIO.openOut input;
    in
        TextIO.closeOut fileOpen
    end
    handle OS.SysErr _ => (print "El archivo no existe en la ruta proporcionada.\n");

end;

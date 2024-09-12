use "Creador.sml";




fun main() = 
    let 
        (* Mostramos el menú *)
        val _ = TextIO.output(TextIO.stdOut, "\n BIENVENIDO AL MENU CREADOR")
        val _ = TextIO.output(TextIO.stdOut, "\n1. Guardar en archivo\n2. Limpiar Archivo\n3. Salir");
        val _ = TextIO.flushOut TextIO.stdOut; (* Forzamos a hacer el print *)
        
        (* Leemos la opción del usuario, procesando la opción de tipo string option *)
        val option = case Creador.outPutM("\nIngrese su opcion: ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""
    in 
        (* Evaluamos la opción ingresada *)
        if option = "1" then
            (
                Creador.saveData();
                print("\nGuardado con éxito!\n");
                main() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "2" then
            (
                Creador.emptyFile();
                print("\nArchivo limpiado con éxito!\n");
                main() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "3" then
            () (* Salimos del programa *)
        else 
            main() (* En caso de opción no válida, volvemos a mostrar el menú *)
    end;

use "Creador.sml";


fun menu_Creador() = 
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
                menu_Creador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "2" then
            (
                Creador.emptyFile();
                print("\nArchivo limpiado con éxito!\n");
                menu_Creador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "3" then
            () (* volvemos al menu *)
        else 
            menu_Creador() (* En caso de opción no válida, volvemos a mostrar el menú *)
    end;



    fun menu_Analizador() = 
    let 
        (* Mostramos el menú *)
        val _ = TextIO.output(TextIO.stdOut, "\n BIENVENIDO AL MENU ANALIZADOR")
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
                menu_Creador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "2" then
            (
                Creador.emptyFile();
                print("\nArchivo limpiado con éxito!\n");
                menu_Creador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "3" then
            () (* volvemos al menu *)
        else 
            menu_Creador() (* En caso de opción no válida, volvemos a mostrar el menú *)
    end;



fun main() = 
    let 
        val _ = TextIO.output(TextIO.stdOut, "\n1. Apartado Creador\n2. Apartado Analizador\n3. Salir\n");
        val _ = TextIO.flushOut TextIO.stdOut; (* Forza hacer el print *)
        
        (* Leemos la opción del usuario *)
        val option = case Creador.outPutM("\nIngrese su opcion: ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""
    in 
        if option = "1" then
            (
                menu_Creador();
                main()  (* Vuelve a mostrar el menú principal después de la operación *)
            )
        else if option = "2" then
            (
                menu_Analizador();
                main()  (* Vuelve a mostrar el menú principal después de la operación *)
            )
        else if option = "3" then
            ()  (* Termina el programa *)
        else 
            main()  (* Si la opción no es válida, vuelve a mostrar el menú *)
    end;

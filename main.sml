use "Creador.sml";
use "Analizador.sml";


fun menu_Creador() = 
    let 
        (* Mostramos el menú *)
        val _ = TextIO.output(TextIO.stdOut, "\n BIENVENIDO AL MENU CREADOR")
        val _ = TextIO.output(TextIO.stdOut, "\n1. Guardar en archivo\n2. Limpiar Archivo\n3. Volver");
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
        val _ = TextIO.output(TextIO.stdOut, "\n1. Mostrar top\n2. Informe de actividades sospechosas\n3. Transacciones por cuenta \n4. Cantidad de transacciones por tipo \n5. Resumen \n6. Volver\n");
        val _ = TextIO.flushOut TextIO.stdOut; (* Forzamos a hacer el print *)

         (* Procesamos el valor que regresa outPutM, asegurándonos de manejar el caso NONE *)
        val archivo = case Creador.outPutM("Ingresa la ruta de tu archivo: ") of
            SOME s => String.substring(s, 0, size s -1)
          | NONE => ""

        
        (* Leemos la opción del usuario, procesando la opción de tipo string option *)
        val option = case Creador.outPutM("\nIngrese su opcion: ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""
        in 
        (* Evaluamos la opción ingresada *)
        if option = "1" then
            (
                
                Analizador.mostrarTopTransacciones(archivo);

                
                menu_Analizador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "2" then
            (
                Analizador.actividadesSospechosas(archivo);
                menu_Analizador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "3" then
            (
                Analizador.transaccionesPorCuenta(archivo);
                menu_Analizador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "4" then
            (
                Analizador.cantidadPorTipo(archivo);
                menu_Analizador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "5" then
            (
                Analizador.generarInforme(archivo);
                menu_Analizador() (* Volvemos a llamar a main para el ciclo del menú *)
            )
        else if option = "6" then
            () (* volvemos al menu *)
        else 
            menu_Analizador() (* En caso de opción no válida, volvemos a mostrar el menú *)
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

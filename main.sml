use "Creador.sml";


fun mostrar_menu_creador () =
    print "\nSeleccione una opción:\n";
    print "1. Agregar nuevo registro bancario\n";
    print "2. Limpiar el índice\n";
    print "3. Salir\n";
    print "Opción: ";

(* Función para leer la opción del menú *)
fun leer_opcion () = 
    case Option.valOf (TextIO.inputLine TextIO.stdIn) of
        "" => 3 (* En caso de error, devuelve la opción de salir *)
      | opcion => valOf(Int.fromString (String.substring(opcion, 0, (size opcion) - 1)))


fun agregar_registro ruta =
    let
        (* Pedir datos al usuario *)
        val _ = print "Número de cuenta origen: ";
        val cuenta_origen = valOf (TextIO.inputLine TextIO.stdIn)
        val cuenta_origen = String.substring(cuenta_origen, 0, size cuenta_origen - 1)

        val _ = print "Fecha y hora (YYYY-MM-DD HH:MM:SS): ";
        val fecha = valOf (TextIO.inputLine TextIO.stdIn)
        val fecha = String.substring(fecha, 0, size fecha - 1)

        val _ = print "Tipo de transacción (deposito, retiro, transferencia): ";
        val tipo = valOf (TextIO.inputLine TextIO.stdIn)
        val tipo = String.substring(tipo, 0, size tipo - 1)

        val _ = print "Monto: ";
        val monto = valOf (TextIO.inputLine TextIO.stdIn)
        val monto = String.substring(monto, 0, size monto - 1)

        val cuenta_destino = 
            if tipo = "trasferencia" then
                (print "Número de cuenta destino: ";
                let val cuenta = valOf (TextIO.inputLine TextIO.stdIn)
                in String.substring(cuenta, 0, size cuenta - 1) end)
            else ""
        
        (* Crear línea de registro *)
        val linea = cuenta_origen ^ "," ^ fecha ^ "," ^ tipo ^ "," ^ monto ^ "," ^ cuenta_destino ^ "\n"
        
        (* Agregar al archivo *)
        val _ = Creador.writeFileA ruta linea
    in
        print "Registro agregado correctamente.\n"
    end;

fun main_creador () =
    let
        (* Leer la ruta del archivo de índice *)
        val _ = print "Introduzca la ruta del archivo índice: ";
        val ruta = valOf (TextIO.inputLine TextIO.stdIn)
        val ruta = String.substring(ruta, 0, size ruta - 1)
        
        (* Ciclo del menú *)
        fun ciclo_creador () =
            let
                val _ = mostrar_menu_creador ()
                val opcion = leer_opcion ()
            in
                case opcion of
                    1 => (agregar_registro ruta; ciclo_creador ())
                  | 2 => (Creador.limpiar_indice ruta; print "Índice limpiado.\n"; ciclo_creador ())
                  | 3 => print "Saliendo...\n"
                  | _ => (print "Opción inválida. Inténtelo de nuevo.\n"; ciclo_creador ())
            end
    in
        ciclo_creador ()
    end;

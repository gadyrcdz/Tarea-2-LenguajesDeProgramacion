use "Creador.sml";
use "Analizador.sml";

fun main () =
   (* Menú principal *)

    let
    val _ = print "1. Agregar nuevo registro\n";
    val _ = print "2. Limpiar índice\n";
    val _ = print "3. Analizar transacciones\n";
    val _ = print "4. Salir\n";
    val _ = print "Seleccione una opción: ";
    val opcion = readInt()
    in
    case opcion of
        SOME 1 => agregarRegistro ()
    | SOME 2 => limpiarIndice ()
    | SOME 3 => menuAnalizador ()
    | SOME 4 => print "Saliendo...\n"
    | _ => print "Opción no válida\n"
    end


(* Llamar a la función main al ejecutar *)
main ();

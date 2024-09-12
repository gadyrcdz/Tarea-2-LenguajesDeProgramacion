use "Creador.sml";

structure Analizador = struct
  (* Funciones del analizador *)
   (* Función de ordenamiento por inserción adaptada para listas de tuplas *)
    fun insertionSortTransacciones(lst: (string * string * string * real * string) list): (string * string * string * real * string) list =
    let
        fun insert (x, []) = [x]
          | insert ((cuenta1, fechaHora1, tipo1, monto1, cuentaDestino1), (cuenta2, fechaHora2, tipo2, monto2, cuentaDestino2)::ys) =
              if monto1 >= monto2 then
                (cuenta1, fechaHora1, tipo1, monto1, cuentaDestino1) :: (cuenta2, fechaHora2, tipo2, monto2, cuentaDestino2) :: ys
              else
                (cuenta2, fechaHora2, tipo2, monto2, cuentaDestino2) :: insert((cuenta1, fechaHora1, tipo1, monto1, cuentaDestino1), ys)
        
        fun sort([], sorted) = sorted
          | sort(x::xs, sorted) = sort(xs, insert(x, sorted))
    in
        sort(lst, [])
    end

    (* Función para leer las transacciones desde el archivo *)
    fun readTransactions archivo =
    let
        (* Función para leer y procesar el archivo *)
        fun procesarArchivo ruta =
            let
                (* Abrimos el archivo *)
                val entrada = TextIO.openIn ruta
                
                (* Función auxiliar para procesar cada línea del archivo *)
                fun procesarLinea () =
                    case TextIO.inputLine entrada of
                        NONE => []
                      | SOME linea =>
                            let
                                (* Procesamos la línea asumiendo que los campos están separados por comas *)
                                val campos = String.fields (fn c => c = #",") linea
                                (* Comprobamos si la lista tiene suficientes campos *)
                                val numCampos = List.length campos
                                (* Devolver una tupla (cuentaOrigen, fechaHora, tipo, monto, cuentaDestino) *)
                                val cuentaOrigen = if numCampos > 0 then List.nth (campos, 0) else ""
                                val fechaHora = if numCampos > 1 then List.nth (campos, 1) else ""
                                val tipo = if numCampos > 2 then List.nth (campos, 2) else ""
                                val monto = case Real.fromString (List.nth (campos, 3)) of
                                    SOME r => r
                                  | NONE => 0.0
                                val cuentaDestino = if numCampos > 4 then List.nth (campos, 4) else ""
                                in
                                    (* Procesamos la línea actual y continuamos con el resto *)
                                    (cuentaOrigen, fechaHora, tipo, monto, cuentaDestino)
                                    :: procesarLinea ()
                            end
                (* Procesamos todas las líneas del archivo *)
                val transacciones = procesarLinea ()
                val _ = TextIO.closeIn entrada
            in
                transacciones
            end
    in
        procesarArchivo archivo
    end

    (* Función para mostrar el ranking de transacciones dentro de un rango *)
    fun mostrarTopTransacciones (archivo: string) =
    let
        (* Leer las transacciones desde el archivo *)
        val transacciones = readTransactions archivo

        (* Solicitar monto mínimo y máximo al usuario *)
        val montoMinInput = case Creador.outPutM("\nIngresa el monto mínimo: ") of
            SOME s => s
          | NONE => ""
        val montoMaxInput = case Creador.outPutM("\nIngresa el monto máximo: ") of
            SOME s => s
          | NONE => ""

        (* Convertir los montos mínimos y máximos a reales *)
        val montoMin = case Real.fromString montoMinInput of
            SOME r => r
          | NONE => 0.0
        val montoMax = case Real.fromString montoMaxInput of
            SOME r => r
          | NONE => 0.0

        (* Filtrar transacciones dentro del rango *)
        val transaccionesFiltradas = List.filter (fn (_, _, _, monto, _) => monto >= montoMin andalso monto <= montoMax) transacciones

        (* Ordenar transacciones por monto en orden descendente *)
        val transaccionesOrdenadas = insertionSortTransacciones transaccionesFiltradas

        (* Función para mostrar una transacción *)
        fun mostrarTransaccion (cuenta, fechaHora, tipo, monto, _) =
            TextIO.output (TextIO.stdOut, 
                (cuenta ^ "\t" ^ fechaHora ^ "\t" ^ tipo ^ "\t" ^ Real.toString monto ^ "\n")
            )
        
    in
        (* Imprimir encabezado *)
        TextIO.output (TextIO.stdOut, "\nRanking de Transacciones por Monto\n");
        TextIO.output (TextIO.stdOut, "Número de Cuenta Origen\tFecha y Hora\tTipo de Transacción\tMonto\n");
        TextIO.output (TextIO.stdOut, "---------------------------------------------------------\n");
        
        (* Mostrar transacciones ordenadas *)
        List.app mostrarTransaccion transaccionesOrdenadas
    end


    
(**********************************************************************************************************************************)



    (* Función para leer las transacciones desde el archivo *)
fun readTransactions archivo =
    let
        (* Función para leer y procesar el archivo *)
        fun procesarArchivo ruta =
            let
                (* Abrimos el archivo *)
                val entrada = TextIO.openIn ruta
                
                (* Función auxiliar para procesar cada línea del archivo *)
                fun procesarLinea () =
                    case TextIO.inputLine entrada of
                        NONE => []
                      | SOME linea =>
                            let
                                (* Procesamos la línea asumiendo que los campos están separados por comas *)
                                val campos = String.fields (fn c => c = #",") linea
                                (* Comprobamos si la lista tiene suficientes campos *)
                                val numCampos = List.length campos
                                (* Devolver una tupla (cuentaOrigen, fecha, tipo, monto, cuentaDestino) *)
                                val cuentaOrigen = if numCampos > 0 then List.nth (campos, 0) else ""
                                val fechaHora = if numCampos > 1 then List.nth (campos, 1) else ""
                                val tipo = if numCampos > 2 then List.nth (campos, 2) else ""
                                val monto = if numCampos > 3 then List.nth (campos, 3) else ""
                                val cuentaDestino = if numCampos > 4 then List.nth (campos, 4) else ""
                                in
                                    (* Procesamos la línea actual y continuamos con el resto *)
                                    (cuentaOrigen, String.substring(fechaHora, 0, 10), tipo, monto, cuentaDestino)
                                    :: procesarLinea ()
                            end
                (* Procesamos todas las líneas del archivo *)
                val transacciones = procesarLinea ()
                val _ = TextIO.closeIn entrada
            in
                transacciones
            end
    in
        procesarArchivo archivo
    end

(* Función para generar el informe de actividades sospechosas *)
fun actividadesSospechosas (archivo: string) =
    let
        (* Leer las transacciones desde el archivo *)
        val transacciones = readTransactions archivo
        
        (* Agrupar transacciones por cuenta y fecha *)
        val transaccionesAgrupadas = List.foldl
            (fn ((cuenta, fecha, _, _, _), acc) =>
                let
                    (* Crear una clave única para la cuenta y la fecha *)
                    val clave = (cuenta, fecha)
                    (* Actualizar el conteo de transacciones en el acumulador *)
                    val updatedAcc = case List.find (fn ((c, f), _) => (c = cuenta andalso f = fecha)) acc of
                        SOME ((_, _), count) => List.filter (fn ((c, f), _) => not (c = cuenta andalso f = fecha)) acc
                                @ [((cuenta, fecha), count + 1)]
                      | NONE => acc @ [((cuenta, fecha), 1)]
                in
                    updatedAcc
                end
            ) [] transacciones
        
        (* Filtrar cuentas y fechas con 5 o más transacciones *)
        val transaccionesSospechosas = List.filter (fn ((_, _), count) => count >= 5) transaccionesAgrupadas
        
        (* Función para mostrar una entrada del informe *)
        fun mostrarSospechosa ((cuenta, fecha), count) =
            print (cuenta ^ "\t" ^ fecha ^ "\t" ^ Int.toString count ^ "\n")
        
    in
        (* Imprimir encabezado *)
        print ("Informe de Actividades Sospechosas\n");
        print ("Número de cuenta origen\tFecha\tNúmero de transacciones\n");
        
        (* Mostrar las transacciones sospechosas *)
        List.app mostrarSospechosa transaccionesSospechosas
    end


(************************************************************************************************************************)
    fun transaccionesPorCuenta (archivo: string) =
    let
        (* Función para leer y procesar el archivo *)
        fun procesarArchivo ruta cuenta =
            let
                (* Abrimos el archivo *)
                val entrada = TextIO.openIn ruta
                
                (* Función auxiliar para procesar cada línea del archivo *)
                fun procesarLinea () =
                    case TextIO.inputLine entrada of
                        NONE => ()  (* Fin del archivo *)
                      | SOME linea =>
                            let
                                (* Procesamos la línea asumiendo que los campos están separados por comas *)
                                val campos = String.fields (fn c => c = #",") linea
                                (* Comprobamos si la lista tiene suficientes campos *)
                                val numCampos = List.length campos
                                val cuentaOrigen = if numCampos > 0 then List.nth (campos, 0) else ""
                                val cuentaDestino = if numCampos > 4 then List.nth (campos, 4) else ""
                                (* Comprobamos si el número de cuenta coincide con el origen o el destino *)
                                val coincide = (cuentaOrigen = cuenta) orelse (cuentaDestino = cuenta)
                            in
                                if coincide then
                                    (* Imprimimos los campos en el formato adecuado *)
                                    TextIO.output(TextIO.stdOut, 
                                        (cuentaOrigen ^ "\t" ^ 
                                         (if numCampos > 1 then List.nth (campos, 1) else "") ^ "\t" ^ 
                                         (if numCampos > 2 then List.nth (campos, 2) else "") ^ "\t" ^ 
                                         (if numCampos > 3 then List.nth (campos, 3) else "") ^ "\t" ^ 
                                         cuentaDestino) ^ "\n")
                                else
                                    ()
                            end
                                (* Llamada recursiva para procesar la siguiente línea *)
                                val _ = procesarLinea ()
                (* Procesamos todas las líneas del archivo *)
                val _ = procesarLinea ()
                val _ = TextIO.closeIn entrada
            in
                ()
            end
        
        (* Solicitamos el número de cuenta al usuario *)
        val cuenta = case Creador.outPutM("\nIngresa el número de cuenta: ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""
        
        (* Mostramos el título *)
        val _ = TextIO.output(TextIO.stdOut, "\nTransacciones para la cuenta: " ^ cuenta ^ "\n")
        val _ = TextIO.output(TextIO.stdOut, "\nOrigen\tFecha\tHora\tTipo\tMonto\tDestino\n")
        val _ = TextIO.output(TextIO.stdOut, "----------------------------------------------------\n")
        
        (* Llamamos a la función para procesar el archivo *)
        val _ = procesarArchivo archivo cuenta
    in
        ()
    end



(**********************************************************************************************************************)


    fun cantidadPorTipo (archivo: string) =
    let
        (* Función para leer y contar las transacciones por tipo *)
        fun contarTipo ruta tipo =
            let
                (* Abrimos el archivo *)
                val entrada = TextIO.openIn ruta
                
                (* Función auxiliar para contar las transacciones del tipo dado *)
                fun contarLinea (contador: int) =
                    case TextIO.inputLine entrada of
                        NONE => contador
                      | SOME linea =>
                            let
                                (* Procesamos la línea según el formato del archivo CSV *)
                                val campos = String.fields (fn c => c = #",") linea
                                (* Supongamos que el tipo de transacción está en el tercer campo *)
                                val tipoTransaccion = if List.length campos > 2 then List.nth (campos, 2) else ""
                                (* Incrementamos el contador si el tipo de transacción coincide *)
                                val nuevoContador = if tipoTransaccion = tipo then contador + 1 else contador
                            in
                                contarLinea nuevoContador
                            end
                (* Contamos las transacciones *)
                val cantidad = contarLinea 0
                (* Cerramos el archivo después de procesar *)
                val _ = TextIO.closeIn entrada
            in
                cantidad
            end
        
        (* Solicitamos el tipo de transacción al usuario *)
        val tipo = case Creador.outPutM("\nIngresa el tipo de transacción: ") of
            SOME s => String.substring(s, 0, size s - 1)
          | NONE => ""
        
        (* Mostramos el resultado *)
        val cantidad = contarTipo archivo tipo
        val _ = TextIO.output(TextIO.stdOut, "\nCantidad de transacciones del tipo " ^ tipo ^ ": " ^ Int.toString(cantidad) ^ "\n")
    in
        ()
    end


(*******************************************)
(* Función para leer las transacciones desde el archivo *)
fun readTransactions archivo =
    let
        (* Función para leer y procesar el archivo *)
        fun procesarArchivo ruta =
            let
                (* Abrimos el archivo *)
                val entrada = TextIO.openIn ruta
                
                (* Función auxiliar para procesar cada línea del archivo *)
                fun procesarLinea () =
                    case TextIO.inputLine entrada of
                        NONE => []
                      | SOME linea =>
                            let
                                (* Procesamos la línea asumiendo que los campos están separados por comas *)
                                val campos = String.fields (fn c => c = #",") linea
                                (* Comprobamos si la lista tiene suficientes campos *)
                                val numCampos = List.length campos
                                (* Devolver una tupla (cuentaOrigen, fecha, tipo, monto, cuentaDestino) *)
                                val cuentaOrigen = if numCampos > 0 then List.nth (campos, 0) else ""
                                val fechaHora = if numCampos > 1 then List.nth (campos, 1) else ""
                                val tipo = if numCampos > 2 then List.nth (campos, 2) else ""
                                val monto = if numCampos > 3 then Real.fromString (List.nth (campos, 3)) else NONE
                                val cuentaDestino = if numCampos > 4 then List.nth (campos, 4) else ""
                                in
                                    (* Procesamos la línea actual y continuamos con el resto *)
                                    (cuentaOrigen, fechaHora, tipo, monto, cuentaDestino)
                                    :: procesarLinea ()
                            end
                (* Procesamos todas las líneas del archivo *)
                val transacciones = procesarLinea ()
                val _ = TextIO.closeIn entrada
            in
                transacciones
            end
    in
        procesarArchivo archivo
    end

(* Función para generar el informe *)
fun generarInforme (archivo: string) =
    let
        (* Leer las transacciones desde el archivo *)
        val transacciones = readTransactions archivo
        
        (* Función para calcular la cantidad de transacciones por tipo *)
        val cantidadPorTipo = List.foldl
            (fn ((_, _, tipo, _, _), acc) =>
                let
                    val updatedAcc = case List.find (fn (t, _) => t = tipo) acc of
                        SOME (_, count) => List.filter (fn (t, _) => t <> tipo) acc @ [(tipo, count + 1)]
                      | NONE => acc @ [(tipo, 1)]
                in
                    updatedAcc
                end
            ) [] transacciones
        
        (* Función para encontrar la transacción con el monto más grande y la más pequeña *)
        val (transaccionMayor, transaccionMenor) = List.foldl
            (fn ((_, _, _, monto, _), (mayor, menor)) =>
                let
                    val actualMayor = case (mayor, monto) of
                        (NONE, SOME m) => SOME m
                      | (SOME m, SOME n) => if m < n then SOME n else mayor
                      | _ => mayor
                    val actualMenor = case (menor, monto) of
                        (NONE, SOME m) => SOME m
                      | (SOME m, SOME n) => if m > n then SOME n else menor
                      | _ => menor
                in
                    (actualMayor, actualMenor)
                end
            ) (NONE, NONE) transacciones
        
        (* Transacciones con monto mayor y menor *)
        val mayorMonto = case transaccionMayor of
            NONE => "No hay transacciones"
          | SOME monto => "Monto mayor: " ^ Real.toString monto
        val menorMonto = case transaccionMenor of
            NONE => "No hay transacciones"
          | SOME monto => "Monto menor: " ^ Real.toString monto
        
        (* Función para calcular la cantidad de transacciones por cuenta *)
        val cantidadPorCuenta = List.foldl
            (fn ((cuenta, _, _, _, _), acc) =>
                let
                    val updatedAcc = case List.find (fn (c, _) => c = cuenta) acc of
                        SOME (_, count) => List.filter (fn (c, _) => c <> cuenta) acc @ [(cuenta, count + 1)]
                      | NONE => acc @ [(cuenta, 1)]
                in
                    updatedAcc
                end
            ) [] transacciones
        
        (* Función para mostrar los resultados *)
        fun mostrarReporte () =
            let
                (* Imprimir cantidad de transacciones por tipo *)
                val _ = print ("Cantidad de transacciones por tipo:\n")
                val _ = List.app (fn (tipo, cantidad) => print (tipo ^ ": " ^ Int.toString cantidad ^ "\n")) cantidadPorTipo
                
                (* Imprimir transacción con monto mayor y menor *)
                val _ = print ("\n" ^ mayorMonto ^ "\n" ^ menorMonto ^ "\n")
                
                (* Imprimir cantidad de transacciones por cuenta *)
                val _ = print ("Cantidad de transacciones por cuenta:\n")
                val _ = List.app (fn (cuenta, cantidad) => print (cuenta ^ ": " ^ Int.toString cantidad ^ "\n")) cantidadPorCuenta
            in
                ()
            end

    in
        mostrarReporte ()
    end

end;
(*******************************************************)
    
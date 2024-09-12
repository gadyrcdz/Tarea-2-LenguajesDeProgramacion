structure Creador = struct
    fun leerArchivo (ruta: string) = 
    let
        val archivo = TextIO.openIn ruta
        val contenido = TextIO.inputAll archivo
        val _ = TextIO.closeIn archivo
    in
        String.tokens Char.isSpace contenido
    end;


    fun writeFileA filename content = 
    let
        val fd = TextIO.openAppend filename
        val _ = (TextIO.output (fd, content)
                 handle e => (TextIO.closeOut fd; raise e))
        val _ = TextIO.closeOut fd
    in
        ()
    end;


    fun limpiar_cambios_linea str = 
    if str = "" then ""
    else if String.sub(str, 0) = #"\n" 
    then limpiar_cambios_linea (String.substring (str, 1, (size str) - 1))
    else String.substring (str, 0, 1) ^
         limpiar_cambios_linea (String.substring (str, 1, (size str) - 1));


    fun mensaje x = (print(x));

    fun limpiar_indice ruta =
    let val _ = TextIO.openOut ruta in () end;


    fun escribir (ruta: string) = 
    let
        val _ = mensaje "Indique un nombre: ";
        val nombre = case TextIO.inputLine TextIO.stdIn of
            SOME temp => limpiar_cambios_linea temp
          | NONE => ""

        val _ = mensaje "Indique el apellido: ";
        val apellido = case TextIO.inputLine TextIO.stdIn of
            SOME temp => limpiar_cambios_linea temp
          | NONE => ""

        val _ = mensaje "Indique la ciudad: ";
        val ciudad = case TextIO.inputLine TextIO.stdIn of
            SOME temp => limpiar_cambios_linea temp
          | NONE => ""

        val linea = nombre ^ "," ^ apellido ^ "," ^ ciudad ^ "\n";
        val _ = writeFileA ruta linea;
    in 
        linea
    end;


end;

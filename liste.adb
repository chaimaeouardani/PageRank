with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Liste is

	----------------------------------------------------------------------------

	procedure Free is
		new Ada.Unchecked_Deallocation (T_Cellule, T_Liste);

	----------------------------------------------------------------------------

	procedure Initialiser(Liste: in out T_Liste) is
	begin
                Liste := Null;
	end Initialiser;

	----------------------------------------------------------------------------

	procedure Detruire (Liste: in out T_Liste) is
		A_Detruire: T_Liste;
	begin
             while Liste /= null loop		
                A_Detruire := Liste;
		Liste := Liste.all.Suivant;
		free(A_Detruire);
             end loop;
	end Detruire;

	----------------------------------------------------------------------------

	procedure Ajouter(Liste:  in out T_Liste; Element: T_Element) is
		nv_liste : T_Liste;
	begin
		nv_liste := new T_Cellule;
                nv_liste.all.Element := Element;
		nv_liste.all.Suivant := Liste;
		Liste := nv_liste;	
	end Ajouter;

	----------------------------------------------------------------------------

	function Premier(Liste: in T_Liste) return T_Element is
	begin
		return Liste.all.Element;
	end;

	----------------------------------------------------------------------------

	function Taille (Liste: in T_Liste) return Integer is
		compteur : Integer := 0;
	begin
		while Liste /= Null loop
			compteur := compteur + 1;
	    end loop;
	    return compteur;
	end Taille;

	----------------------------------------------------------------------------
	
	function Est_Present (Liste: in T_Liste; Element: in T_Element) return Boolean is
	        l : T_Liste := Liste;
        begin
		
             if Liste = Null then return False;
             else
                
                while l.all.Suivant /= null and then l.all.Element/= Element loop
			l := l.all.Suivant;
		end loop;
		if l.all.Element = Element then 
				return True;
		else 
				return False;
                end if; 
             end if;
	end Est_Present;

	----------------------------------------------------------------------------
end Liste;

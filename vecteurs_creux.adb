with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Integer_Text_IO;         use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;

package body Vecteurs_Creux is

	procedure Free is
		new Ada.Unchecked_Deallocation (T_Cellule, T_Vecteur_Creux);



	procedure Initialiser (V : out T_Vecteur_Creux) is
	begin
		V := null;
	end Initialiser;


	procedure Detruire (V: in out T_Vecteur_Creux) is
        memoire : T_Vecteur_Creux;
        begin
            while V /= Null loop
                memoire := V.suivant;
                free(V);
                V := memoire;
             end loop;

	end Detruire;


	function Est_Nul (V : in T_Vecteur_Creux) return Boolean is
	begin
		return V =  Null;
	end Est_Nul;


        function Composante_Recursif (V : in T_Vecteur_Creux ; Indice : in Integer) return T_Double is
        begin

           if Est_Nul(V) then
             return 0.0;


           elsif V.all.Indice = Indice  then
              return V.all.Valeur;
           else return Composante_Recursif(V.all.Suivant , Indice);
           end if;

        end Composante_Recursif;


	function Composante_Iteratif (V : in T_Vecteur_Creux ; Indice : in Integer) return T_Double is
              val : T_Double := 0.0;
              l : T_Vecteur_Creux := V;

        begin

          while not Est_Nul(l) and l.all.Indice <= Indice loop
             if l.all.Indice = Indice then
                 val := l.all.Valeur;
             else
                 l := l.all.Suivant;
             end if;
          end loop;
          return val;


	end Composante_Iteratif;




        procedure Modifier (V : in out T_Vecteur_Creux ;
				       Indice : in Integer ;
					   Valeur : in T_Double ) is
	l : T_Vecteur_Creux := V;
        begin


             l := new T_Cellule;
             l.all.Indice := Indice;
             l.all.Valeur := Valeur;
             l.suivant := V;
             V := l;


	end Modifier;


	function Sont_Egaux_Recursif (V1, V2 : in T_Vecteur_Creux) return Boolean is
	begin
          if V1 = Null and V2 = Null then
              return True;

          elsif V1.Valeur = V2.Valeur and V1.Indice = V2.Indice then
               return Sont_Egaux_Recursif(V1.all.Suivant , V2.all.Suivant);

          else
               return False;
          end if;
	end Sont_Egaux_Recursif;


        function Sont_Egaux_Iteratif (V1, V2 : in T_Vecteur_Creux) return Boolean is
          l1 , l2 : T_Vecteur_Creux;
        begin
            l1 := V1;
            l2 := V2;
             while l1.Indice = l2.Indice and l1.Valeur = l2.valeur loop
                  l1 := l1.all.Suivant;
                  l2 := l2.all.Suivant;

             end loop;
             if l1 = Null and l2 = Null then
                  return True;
             else
                  return False;

             end if;

        end Sont_Egaux_Iteratif;


	procedure Additionner (V1 : in out T_Vecteur_Creux; V2 : in T_Vecteur_Creux) is


        begin
            while V1.all.Suivant /= Null loop
               V1 := V1.all.Suivant;
            end loop;
            V1.all.suivant := V2;

	end Additionner;



	function Nombre_Composantes_Non_Nulles (V: in T_Vecteur_Creux) return Integer is
	begin
		if V = Null then
			return 0;
		else
			return 1 + Nombre_Composantes_Non_Nulles (V.all.Suivant);
		end if;
	end Nombre_Composantes_Non_Nulles;


end Vecteurs_Creux;


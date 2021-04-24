with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ADA.Long_Float_Text_IO; use ADA.Long_Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Liste;
with Vecteurs_Creux; use Vecteurs_Creux;

procedure Pagerank_principale is

    package Liste_entiers is new Liste(Integer);
    use Liste_entiers;


    Type T_vect is array(integer range <>) of T_Vecteur_Creux;
    Type T_creux is access T_vect;

    type T_Matrice is array(integer range <> , integer range <>) of T_Double;
    type T_pleine is access T_Matrice;

    Type T_Tab_Liste is array (integer range <>) of T_Liste;
    type T_tl_access is access T_Tab_Liste;

    type T_tab_double is array(integer range <>) of T_Double;
    type T_td_access is access T_tab_double;

    type T_Hyperliens is record
         --conserver les pages referencie pour eviter les doublons et voir la page i reference la page j dans le calcul de G(i,j)
         pages : T_tl_access;
         --Conserver le nombre des hyperliens de chaque page
         nombre : T_td_access;
    end record;

    type T_classement is record
         --pointeur sur un tableau des numeros de pages
         id : T_td_access;
         --pointeur sur un tableau des rangs de chaque page
         poids : T_td_access;
    end record;


    --Initialiser les parametres par defaut
      nb_iter : Integer := 150;
      alpha : T_Double := 0.85;
      utiliser_matrice_creuse : Boolean := True;
      reseau : Unbounded_String;

      Fichier_Manquant : Exception;


      Hyperliens : T_Hyperliens;
      Poids : T_td_access;
      Classement : T_classement;
      H_naive : T_pleine;
      H_creux : T_creux;
      N_pages : integer;


      procedure Free_td is
		new Ada.Unchecked_Deallocation(T_tab_double, T_td_access);

      procedure Free_tl is
		new Ada.Unchecked_Deallocation(T_Tab_Liste, T_tl_access);

      procedure Free_naive is
		new Ada.Unchecked_Deallocation(T_Matrice, T_pleine);

      procedure Free_creux is
		new Ada.Unchecked_Deallocation(T_vect, T_creux);

     procedure afficher_utilisation is
      begin
         Put_Line("Méthode d'utilisation :"&Command_Name&" [-P] [-I nb_iteration] [-A alpha] fichier");
         Set_Exit_Status(Failure);
      end afficher_utilisation;

      procedure recuperer_parametres is
        i : Integer := 1;
        arg : Unbounded_String;

      begin
        if Argument_Count < 1 then
            raise Fichier_Manquant;
        end if;

        while i <= Argument_Count-1 loop
            arg := To_Unbounded_String(Argument(i));
            if arg="-P" or else arg="-p" then
                utiliser_matrice_creuse := False;
            elsif arg="-I" or else arg="-i" then
                begin
                    nb_iter := Integer'Value(Argument(i+1));
                    i:= i+1;
                exception
                    when Constraint_Error => Put_Line("le nombre d'iteration doit etre un entier");
                                             raise Constraint_Error;
                end;
            elsif arg = "-A" or else arg ="-a" then
                begin
                    alpha := T_Double'Value(Argument(i+1));
                    i := i+1;
                exception
                    when Constraint_Error => Put_Line("alpha doit ètre un flottant");
                        raise Constraint_Error;
                end;

            else
                Put(To_String(arg));
                Put(" n'est pas un paramètre valide");
                raise Constraint_Error;
            end if;
            i := i + 1;
        end loop;

        reseau := To_Unbounded_String(Argument(Argument_Count));




    end recuperer_parametres;


     --Initialiser un enregistrement qui regroupe les pages referencie et le nombres des hyperliens de chaque page
     procedure Initialiser_hyp(hyp : in out T_Hyperliens ; N : in Integer) is
      begin
                hyp.pages := new T_Tab_Liste(0..N-1);
                hyp.nombre := new T_tab_double(0..N-1);
                for i in 0..N-1 Loop
                      hyp.nombre.all(i) := 0.0;
                      Initialiser(hyp.pages.all(i));
                end loop;

     end;

     --obtenir une liste et le nombre des hyperliens de chaque pages
     procedure Calculer_hyperliens(reseau : IN Unbounded_String ; Hyperliens :  IN OUT T_Hyperliens ; N_pages : out integer) is
		 pg_origine , pg_referencie  : Integer;
                 Nom_fichier : 	File_Type;
     begin

                --Ouvrir le fichier .net
                Open(Nom_fichier , In_File, To_String(reseau));
                --Recuperer le nombre des pages;
                Get(Nom_fichier , N_pages);
                --Initialiser hyperliens
                Initialiser_hyp(Hyperliens , N_pages);
                while not End_Of_File(Nom_fichier) loop
                        --Recuperer la page d'orgine et la page qu'elle referencie
			Get(Nom_fichier, pg_origine);
			Get(Nom_fichier, pg_referencie);
                        --Enregistrer les donnees
			if  not Est_Present(Hyperliens.pages.all(pg_origine) , pg_referencie) then
				Ajouter(Hyperliens.pages.all(pg_origine) , pg_referencie);
                                Hyperliens.nombre.all(pg_origine) := Hyperliens.nombre.all(pg_origine) + 1.0;
			end if;
                end loop;
                --Fermer le fichier
                Close(Nom_fichier);



      end;



     --pour implementatione naive
     procedure calculer_S_naive(H : in out T_pleine) is
        N_hyperliens: T_Double;
        begin

           for i in 0..N_pages-1 loop
              N_hyperliens := Hyperliens.nombre.all(i);

              if N_hyperliens /= 0.0 then
                 for j in 0..N_pages-1 loop
                    --Verifier si la page i reference la page j
                    if Est_Present(Hyperliens.pages.all(i) , j) then
                          H.all(i,j) := 1.0/N_hyperliens;
                    else
                          H.all(i,j) := 0.0;
                    end if;
                 end loop;
              --Remplacer la ligne puit par 1/N_pages
              else
                 for j in 0..N_pages-1 loop
                    H.all(i,j) := 1.0/T_Double(N_pages);
                 end loop;

              end if;
           end loop;
      end calculer_S_naive;

      procedure Calculer_G_naive(S : IN OUT T_pleine) is
       begin
            for i in 0..N_pages-1 loop
                for j in 0..N_pages-1 loop
                     S.all(i,j) := alpha*S.all(i,j) + (1.0-alpha)/T_Double(N_pages);
                end loop;
            end loop;

       end calculer_G_naive;

       --pour implementation creuse
       procedure calculer_H_creux(H : in out T_creux) is
        N_hyperliens: T_Double;
        begin

           for i in 0..N_pages-1 loop
              N_hyperliens := Hyperliens.nombre.all(i);
	      --Initialiser(H.all(i));
              if N_hyperliens /= 0.0 then
                 for j in 0..N_pages-1 loop
                    --Verifier si la page i reference la page j
                    if Est_Present(Hyperliens.pages.all(i) , j) then
                         Modifier(H.all(i), j , T_Double(1.0/N_hyperliens));

                    end if;
                 end loop;

              end if;
           end loop;

      end calculer_H_creux;



      function G_ij_creux(H : in T_creux; i: in integer ; j : in integer ) return T_Double is
           G_ij : T_Double;
      begin
           if Est_Nul(H.all(i)) then
                G_ij := 1.0/T_Double(N_pages);
           else
                G_ij := alpha*Composante_Recursif(H.all(i), j) + (1.0-alpha)/T_Double(N_pages) ;
           end if;
           return G_ij;
      end;


      --Initialiser un vecteur a 1/N_pages;
      procedure Initialiser_poids(vecteur : OUT T_td_access; N_pages : in integer) is
      begin
           vecteur := new T_tab_double(0..N_pages-1);
           for i in 0..N_pages-1 loop
                vecteur.all(i) := 1.0/T_Double(N_pages);
           end loop;
      end;

      --Recuperer les poids
      Function Poids_page( N_iter : IN Integer ; N : IN integer) return T_td_access is
          poids : T_td_access;
          nv_poids : T_td_access;
          somme : T_Double;
          G_ij : T_Double;
      begin
          --Initialiser poids a 1/N_pages
          Initialiser_poids(poids , N);
          Initialiser_poids(nv_poids , N);

          --calculer les iterations
       	  for k in 1..N_iter loop
              --calculer produit Pi = Pi*G
              Put_Line("debut");
              for j in 0..N-1 loop
                  somme := 0.0;
                  for i in 0..N-1 loop

                       if utiliser_matrice_creuse then
                           G_ij := G_ij_creux(H_creux , i , j);

                       else
                           G_ij := H_naive.all(i,j);
                       end if;

                       somme := somme +  poids.all(i) * G_ij;
                  end loop;
                  nv_poids.all(j) := somme;
              end loop;
              poids.all := nv_poids.all;
              Put_Line("fin");
          end loop;
          Free_td(nv_poids);
          return poids;
      end;

      procedure permuter(Classement : in out T_classement ; i,j : in integer) is
                  temp1 : T_Double := Classement.id.all(i);
                  temp2 : T_Double := Classement.poids.all(i);
      begin

                  Classement.poids.all(i) := Classement.poids.all(j);
                  Classement.id.all(i) := Classement.id.all(j);
                  Classement.poids.all(j) := temp2;
                  Classement.id.all(j) := temp1;
      end permuter;

      procedure Trier(Classement : in out T_classement) is

             Max : integer;

      begin
             for i in 0..N_pages-1 loop
                  Max := i;
                  --obtenir l'indice de poids maximal
                  for j in i+1..N_pages-1 loop
                      if Classement.poids.all(j) > Classement.poids.all(Max) then
                          Max := j;
                      end if;
                  end loop;
                  permuter(Classement, Max , i);
              end loop;
      end;


     --Creer les fichiers et les remplir
     procedure ecrire_resultats(Classement : In T_classement) is
        F_Rangs : File_Type;
        F_Poids : File_Type;
    begin
        Create(File => F_Rangs,
               Mode => Out_File,
               Name => To_String(reseau) & "_page.ord",
               Form => "");
        Create(File => F_Poids,
               Mode => Out_File,
               Name => To_String(reseau) & "_rangs.p",
               Form => "");
        Put(F_poids, N_pages , 1);
        put(F_Poids, " ");
        Put(F_Poids, Long_Float(alpha) , Fore => 1 , Aft => 10);
        Put(F_Poids, Integer'Image(nb_iter));
        New_Line(F_poids , 1);
        for i in 0..integer(N_pages)-1 loop
            Put(File => F_Poids, Item => Long_Float(Classement.poids.all(i)) , Fore=>1, Aft=>10);
            New_Line(F_Poids ,1);
            Put(File => F_Rangs,  Item => Integer(Classement.id.all(i)) , Width => 1);
            New_Line(F_Rangs ,1);

        end loop;
        Close(F_Rangs);
        Close(F_Poids);

    end ecrire_resultats;



begin
    recuperer_parametres;
    if Length(reseau) = 0 then
        raise fichier_manquant;
    end if;
    Put_Line("-- Paramètres utilisés --");
    Put_Line(" -Matrice Creuse: "&Boolean'Image(utiliser_matrice_creuse));
    Put_Line(" -Itérations:"&Integer'Image(nb_iter));
    Put_Line(" -Alpha:"& T_Double'Image(alpha));
    Put_Line(" -Fichier: "&To_String(reseau));
    Put_Line("-------------------------");

    Put_Line("En cours de génération des fichiers ... ");

    Calculer_hyperliens(reseau , Hyperliens, N_pages);

    if not utiliser_matrice_creuse then
         H_naive := New T_Matrice(0..N_pages-1, 0..N_pages-1);
        --construire G
         calculer_S_naive(H_naive);
         Calculer_G_naive(H_naive);
    else
         H_creux := New T_vect(0..N_pages-1);
         calculer_H_creux(H_creux);
    end if;
    --obtenir la liste des poids
    Poids := Poids_page(nb_iter , N_pages);
    --test_H(H_creux);

    Classement.id := new T_tab_double(0..N_pages-1);
    Classement.poids := new T_tab_double(0..N_pages-1);

    --Obtenir Classement
    for i in 0..N_pages-1 loop
        Classement.id.all(i) := T_Double(i);
    end loop;

    Classement.poids.all := Poids.all;

    --Trier les pages par poids decroissant
    Trier(Classement);


    --Generer les fichiers
    ecrire_resultats(Classement);
    Put_Line("Les deux fichiers .p et .ord sont generés avec succés");

    for i in 0..N_pages-1 loop
        Detruire(Hyperliens.pages.all(i));
    end loop;

    if not utiliser_matrice_creuse then
             Free_naive(H_naive);
    else
             Free_creux(H_creux);
    end if;

    Free_tl(Hyperliens.pages);
    Free_td(Hyperliens.nombre);
    Free_td(Classement.poids);
    Free_td(Classement.id);
    Free_td(Poids);


exception
    when fichier_manquant => Put_Line("Veuillez indiquer un ficher.");
                             afficher_utilisation;
    when Constraint_Error => afficher_utilisation;
    when Name_Error => Put_Line("Votre fichier est introuvable");

    when others => Put_Line("Une erreur inattendue est survenue!");
end Pagerank_principale;

(self: super: 
  { picom = super.picom.overrideAttrs (old: rec 
     { pname = "picom"; 
        version = "8.3"; 
        src = super.fetchFromGitHub {
          owner = "pijulius";
          repo = "picom"; 
          rev = "982bb43e5d4116f1a37a0bde01c9bda0b88705b9"; 
          sha256 = "YiuLScDV9UfgI1MiYRtjgRkJ0VuA1TExATA2nJSJMhM=";
         }; 
       patches = []; 
     }); 
} )

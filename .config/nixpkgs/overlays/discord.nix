 (self: super: { 
    discord = super.discord.overrideAttrs (_: { src = builtins.fetchTarball "https://dl.discordapp.net/apps/linux/0.0.24/discord-0.0.24.tar.gz" ; });
    }
    )


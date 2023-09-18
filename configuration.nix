# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
user = "Sammy";

in
{
	imports =
		[ # Include the results of the hardware scan.
		./hardware-configuration.nix
			<home-manager/nixos>
		];
	nixpkgs = {
		overlays = [
#(import /home/Sammy/.config/nixpkgs/overlays/awesome.nix)
#(import /home/Sammy/.config/nixpkgs/overlays/picom.nix)
			#(import /home/Sammy/.config/nixpkgs/overlays/discord.nix)
				(import (builtins.fetchTarball {
					 url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
					 }))

				];
	};

# Use the GRUB 2 boot loader.
	boot.loader = {
		grub.configurationLimit = 5;
#	grub = {
#		enable = true;
#		version = 2;
#	};
		systemd-boot = {
			enable = true;
		};
		efi = {
			canTouchEfiVariables = true;
		};
	};
# boot.loader.grub.efiSupport = true;
# boot.loader.grub.efiInstallAsRemovable = true;
# boot.loader.efi.efiSysMountPoint = "/boot/efi";
# Define on which hard drive you want to install Grub.
# boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

	networking.hostName = "Xos"; # Define your hostname.
# Pick only one of the below networking options.
# networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
	networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

# time zone.
	time.timeZone = "Africa/Algiers";
# location
	location.provider = "geoclue2";

# PowerManagment
	powerManagement = {
		enable = true;
		powertop.enable = true;
	};
	

# Fonts Baby
	fonts = {
		fonts = with pkgs; [
			(nerdfonts.override {fonts = 
			 ["FiraCode"
			 ]; })

		];
		fontconfig = {
			defaultFonts = {
				serif = ["FiraCode Nerd Font"];

			};
		};
	};


# Configure network proxy if necessary
# networking.proxy.default = "http://user:password@proxy:port/";
# networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

# Select internationalisation properties.
	i18n.defaultLocale = "en_US.UTF-8";
# console = {
#         font = "Lat2-Terminus16";
#         keyMap = "us";
#         useXkbConfig = true; # use xkbOptions in tty.
# };

# Enable the X11 windowing system.
# services.xserver.enable = true;
	services.xserver = {
		enable = true;
		libinput.enable = true;
		layout = "us,ar";
		xkbOptions = "ctrl:swapcaps,grp:shifts_toggle";

#videoDrivers = ["amdgpu"];

		displayManager = {
			lightdm = {
				enable = true;
#			greeter = {
#				enable = true;
#				package = pkgs.lightdm-enso-os-greeter;
#				};
				greeters = {
					slick = {
						enable = true;
					};

				};
			};
			defaultSession = "none+i3";
		};
		windowManager = {
			awesome = {
				enable = true;
				luaModules = with pkgs;[luarocks];
			};
			herbstluftwm = {
				enable = true;
			};
			spectrwm = {
				enable = true;
			};
			i3 = {
				enable = true;
				extraPackages = with pkgs; [
					i3status
						i3lock
				];
				package = pkgs.i3-gaps;
			};
		};
		desktopManager = {
			xfce = {
				enable = true;
			};
		};
	};



# Enable CUPS to print documents.
# services.printing.enable = true;
# Enable sound.
	sound.enable = true;
	hardware = {
		opengl = {
			enable = true;
			driSupport = true;
			driSupport32Bit = true;
			extraPackages = with pkgs; [
				amdvlk
			];
		};
	};


# Define a user account. Don't forget to set a password with ‘passwd’.
	users= {
		mutableUsers = true;
		users.${user} = {
			isNormalUser = true;
			group = "users";
			extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
				packages = with pkgs; [
				brave
				];
			shell = pkgs.zsh;
		};
		extraGroups.vboxusers.members = ["Sammy"];
	};

# List packages installed in system profile. To search, run:
# $ nix search wget
	environment.variables = {
		EDITOR = "nvim";
	};
	environment.pathsToLink = [ "/share/zsh" ];

	environment.systemPackages = with pkgs; [
		wget
			discord
			gcc
			llvmPackages_15.clang-unwrapped
			killall
			fd
			libnotify
			pulseaudio
			brightnessctl
			unixtools.procps
			gparted
			zscroll
			ninja
			acpi
			acpid
			linuxKernel.packages.linux_zen.acpi_call
			jq
			toybox
			lshw
			libmpdclient
			upower
			lxappearance
			inotify-tools
			polkit_gnome
			xdotool
			go-mtpfs
			lsof
			firefox
			ranger
			xclip
			picom-jonaburg
			gpick
			ffmpeg
			blueman
			maim
			mpv
			mpc-cli
			thunderbird
			mutagen
			playerctl
			genymotion
			unzip
			libsecret
			exa
			bat
			tmux
			tmuxinator
			virt-manager
#virtualbox
			OVMF
			insomnia
			libvirt
			libsForQt5.kdenlive
			qemu
			geoclue2
			neovim
			curl
			glxinfo
			calibre
			odt2txt
			mediainfo
			djvulibre
			poppler_utils
			lynx
			libcaca
			libarchive
			texlive.combined.scheme-full
			redis
			python311
			sherlock
# gui
			papirus-icon-theme
			arc-theme
			nordzy-cursor-theme
			];

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
# programs.mtr.enable = true;
	programs = {
		gnupg = {
			agent = {
				enable = true;
				enableSSHSupport = true;
			};
		};
		adb = {
			enable = true;
		};  
		starship = {
			enable = true;
			settings = {
				hostname = {
					ssh_only = false;
					disabled = false;
				};
				os =  {
					disabled = false;
				};
			};
		};
	};

# List services that you want to enable:

	security = {
		rtkit = {
			enable = true;	
		};

	};

	services = {
		pipewire = {
			enable = true;
			alsa = {
				enable = true;
				support32Bit = true;
			};
			pulse.enable = true;
		};
		mysql = {
			enable = true;
			package = pkgs.mariadb;
		};
		redis = {
			package = pkgs.redis;
		};
		redshift = {
			enable = true;
			brightness = {
				night = "0.5";
				day = "1.0";
			};
		};
# bluetooth
		blueman.enable = true;
# Enable the OpenSSH daemon.
		openssh = {
			enable = true;
		};

		geoclue2 = {
			enable = true;
		};
		mpd = {
			enable = true;
			user = "Sammy";
			musicDirectory = /home/Sammy/Music;
			dataDir = /home/Sammy/.config/mpd;
			network.listenAddress = "any";
			extraConfig = ''
				music_directory 	"/home/Sammy/Music"
				playlist_directory	"/home/Sammy/.config/mpd/playlists"
				state_file	"/home/Sammy/.config/mpd/mpd.state"
				sticker_file	"/home/Sammy/.config/mpd/sticker.sql"
				db_file 	"/home/Sammy/.config/mpd/database"

				audio_output {
					type "alsa"
						name "My ALSA"
						device			"hw:0,0"	# optional 
						format			"44100:16:2"	# optional
						mixer_type		"hardware"
						mixer_device	"default"
						mixer_control	"PCM"
				}

			audio_output {
				type                   "fifo"
					name                   "Visualizer"
					format                 "44100:16:2"
					path                   "/tmp/mpd.fifo"
			}

			audio_output {
				type	"httpd"
					name	"My HTTP Stream"
					encoder	"lame"
					port	"8000"
					quality	"5.0"
					format	"44100:16:1"
					always_on	"yes"
					tags	"yes"
					max_clients	"0"
			} '' ;

		};
		postgresql = {
			enable = true;
			package = pkgs.postgresql_15;
		};
	};

# Virtualization
	virtualisation = {
		libvirtd = {
			enable = true;
			qemu = {
				ovmf.enable = true;
			};

		};
		waydroid.enable = true;
		virtualbox.host.enable = true;
	};




# Programs Config
	programs = {
		zsh = {
			enable = true;
			enableCompletion = true;
			autosuggestions.enable = true;
			syntaxHighlighting.enable = true;

		};
		fzf = {
			fuzzyCompletion = true;
			keybindings = true;
		};
		git = {
			enable = true;
		};


	};


#nixpkgs conf
	nixpkgs.config = {allowUnfree = true;};

# System Config
	system.autoUpgrade = {
		enable = true;
		channel = https://nixos.org/channels/nixos-unstable;
		dates = "monthly";
		operation = "boot";
	};

# HOME MANAGER
	home-manager.users.${user} = {pkgs, ...}: {
		nixpkgs.config = {allowUnfree = true;};
		home.packages = with pkgs;[

			kitty
				tor-browser-bundle-bin
				alacritty
				lame
				pass
				sqlite
				sqlite-web
				polybarFull
				tree-sitter
				rustup
				gimp
				xscast
				kazam
				sxhkd
				xplr
				lua-language-server
				stylua
				mate.engrampa
				noto-fonts-extra
				python310Packages.flake8
				python310Packages.uvicorn
				python310Packages.pysdl2
				python310Packages.ptpython
				python310Packages.pywal
				python310
				anki
				ruby_3_1
				poetry
				fontforge
				picard
				obsidian
				zotero
				sioyek
				mmv
				zsh-powerlevel10k
				spotify
				element-desktop
				tdesktop
				hyperfine
				spotdl
				nodePackages.browser-sync
				rofi
				rofi-calc
				btop
				nitrogen
				nix-serve
				audacity
				blender
#foxitreader
#DEV
				lua5_1
#luajit
#python311
#python310
				python310Packages.pip
#python310Packages.poetry
#python310Packages.bpython
#python310Packages.ipython
#python310Packages.uvicorn
#python310Packages.typer
				virtualenv
				black
				mypy
				nodejs
				nodePackages.eslint
				nodePackages.typescript
				nodePackages.npm
				];

		home.file.".icons/default".source = "${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ";
		accounts.email.accounts = {
			gmail = {
				primary = true;
				address = "samidgallabi45@gmail.com";
				realName = "Sami Djellabi";
				userName = "samidgallabi45";
				neomutt.enable = true; 
				passwordCommand = "echo 'jesuismohamaD1'";
				imap = {
					host = "imap.gmail.com";
					port = 993;
					tls.enable = true;
				};
				smtp = {
					host = "smtp.gmail.com";
					port = 465;
					tls.enable = true;
				};

			};

		};

		programs = {
			ncmpcpp = {
				enable = true;
				package = pkgs.ncmpcpp.override { visualizerSupport = true; };
				mpdMusicDir = /home/Sammy/Music;
				settings = {
					cyclic_scrolling = "yes";
					mouse_support = "yes";
					visualizer_data_source = "/tmp/mpd.fifo";
					visualizer_output_name = "Visualizer";
					visualizer_in_stereo = "no";
					visualizer_type = "ellipse";
					visualizer_fps = "60";
					visualizer_look = "●▮";
					visualizer_color = "33,39,63,75,81,99,117,153,189";
					execute_on_song_change = "/home/Sammy/.local/bin/songinfo";


				};
			};
			zsh = {
				enable = true;
				enableCompletion = true;
				enableAutosuggestions = true;
				enableSyntaxHighlighting = true;
				autocd = true;
				dotDir = ".config/zsh";
				initExtra = ''# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
# Not supported in the "fish" shell.
					(cat ~/.cache/wal/sequences &)
# Alternative (blocks terminal for 0-3ms)
					cat ~/.cache/wal/sequences
# To add support for TTYs this line can be optionally added.
					source ~/.cache/wal/colors-tty.sh
					zstyle ":completion:*" matcher-list "" 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-       z}' 'r:|[._-]=* r:|=* l:|=*' '';
				history = {
					save = 100000;
				};
				shellAliases = {
					v = "nvim";
					vi = "nvim";
					vim = "nvim";
					py = "python";
					t = "tmuxinator";
					vimp = "nvim ~/.config/nvim";
					rn= "ranger";

				};
			};
			kitty = {
				enable = true;
				font = {
					name = "FiraCode Nerd Font";
					size = 13;
				};
				extraConfig = "background_opacity 0.7";

#theme = "Catppuccin-Mocha";
			};
			fzf = {
				enable = true;
				enableZshIntegration = true;
				changeDirWidgetCommand = "fd --type f --color=never";
				changeDirWidgetOptions = ["--preview 'tree -C {} | head -50'"];
				defaultOptions = ["--height=100%"
					"--color=bg+:#343d46,gutter:-1,pointer:#ff3c3c,info:#0dbc79,hl:#0dbc79,hl+:#23d18b"
				];
                fileWidgetOptions = ["--preview 'bat --color=always --line-range :50 {}'"];
				fileWidgetCommand =  "fd --type f --color=never";
				tmux = {
					enableShellIntegration = true;
					shellIntegrationOptions = ["-d 40%"];
				};
			};
			direnv = {
				enable = true;
				enableZshIntegration = true;
#config = {};
				nix-direnv.enable = true;
			};
			browserpass = {
				enable = true;
				browsers = ["brave"];
			};
			neomutt = {
				enable = true;
			};

		};
		services = {
			dunst = {
				enable = true;
				iconTheme = {
					package =  pkgs.gnome.adwaita-icon-theme;
					name = "Adwaita";
				};
				settings = {
					global = {
						monitor = 0;
						always_run_script = true;
						timeout = 10;
						background = "#282a36";
						foreground = "#f8f8f2";
						enable_posix_regex = false;
						notification_limit = 2;
						origin = "top-right";
						offset = "30x50";
						progress_bar = true;
						font = "JetBrains Mono 10";
						icon_theme = "Adwaita";
						corner_radius = 2;
					};
					urgency_normal = {
						foreground = "#f8f8f2";
						background = "#6272a4";
					};
					urgency_low = {
						foreground = "#f8f8f2";
						background = "#6272a4";
					};
					urgency_critical = {
						foreground = "#ff5555";
						background = "#282a36";
					};
					xspeak = {
						summary = "*";
						script = "/home/Sammy/dev/shell/alert.sh";
					};
					spotify = {
						appname = "Spotify";
						background = "#2b2422";
						foreground = "#54d383";
					};

				};
			};
			betterlockscreen = {
				enable = true;
				arguments = [
					"-u /home/Sammy/Downloads/nix-wallpaper-simple-red.png"
						"--blur 0.6"
						"-w"
				];
				inactiveInterval = 15;
			};
			syncthing = {
				enable = true;
			};
			xidlehook = {
				enable = true;
				not-when-fullscreen = true;
				not-when-audio = true;
			};
#	mpdris2 = {
#		enable = true;
#		multimediaKeys = true;
#		notifications = true;
#		mpd = {
#			password = "toortoor";
#		};
#	};
			lorri = {
				enable = true;
				enableNotifications = true;
			};
		};
		home.stateVersion = "22.05";
	};

# nix Configuration
	nix = {
		package = pkgs.nixUnstable;
		settings = {
			trusted-users = [
				"${user}"
			];

		};
		extraOptions = ''
			experimental-features = nix-command flakes
			'';
	};

# Open ports in the firewall.
	networking.firewall.enable = true;
	networking.firewall.allowedTCPPorts = [ 6060 8000 ];
# networking.firewall.allowedUDPPorts = [ ... ];
# Or disable the firewall altogether.

# Copy the NixOS configuration file and link it from the resulting system
# (/run/current-system/configuration.nix). This is useful in case you
# accidentally delete configuration.nix.
# system.copySystemConfiguration = true;

# This value determines the NixOS release from which the default
# settings for stateful data, like file locations and database versions
# on your system were taken. It‘s perfectly fine and recommended to leave
# this value at the release version of the first install of this system.
# Before changing this value read the documentation for this option
# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
	system.stateVersion = "22.11"; # Did you read the comment?

}


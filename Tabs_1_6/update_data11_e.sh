## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs6
sudo mkdir /srv/shiny-server/Abs6


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs6

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs6/ /srv/shiny-server/Abs6 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs6

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
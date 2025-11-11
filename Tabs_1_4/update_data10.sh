## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs4
sudo mkdir /srv/shiny-server/Abs4


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs4

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs4/ /srv/shiny-server/Abs4 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs4

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server

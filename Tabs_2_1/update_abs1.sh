## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs1
sudo mkdir /srv/shiny-server/Abs1


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs1

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs1/ /srv/shiny-server/Abs1 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs1

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server

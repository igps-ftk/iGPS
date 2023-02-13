import email
import os

prog='eml2txt.py'

path = '/home/tianyf/livemail/163 (igps_s dbf/Inbox/'
listing = os.listdir(path)

for fle in listing:
    #print fle
    if str.lower(fle[-4:])==".eml":
        #print "here", fle
        #print path+fle
        msg = email.message_from_file(open(path+fle))
        #print "subject:", msg['subject']
        #print "msg:",msg
        #txt = msg.get_body(preferencelist=('plain')).get_content()
        for part in msg.walk():
            #print "||NEW part"
            #print part.get_content_type()
            if part.get_content_type()=="text/plain":
                txt=part.get_payload(decode=True)
                #print "txt:",txt
            
        ofile = path+os.path.splitext(fle)[0]+'.txt'
        #print "output to",ofile
        if os.path.isfile(ofile):
            print "["+prog+"]INFO: ("+ofile+")"
            print "["+prog+"]INFO: output file already exist. skipped"
            continue
            
        f=open(ofile,"w")
        f.write("From: "+ msg['from']+"\r\n")
        f.write("Subject: "+ msg['subject']+"\r\n")
        #f.write("msg\n")
        f.write(txt)
        f.close()
        #with open(ofile,'w') as f:
        #    print('Filename:',file=f)
        
        
        attachments=msg.get_payload()
        
        for attachment in attachments:
            try:
                fnam=attachment.get_filename()
                f=open(fnam, 'wb').write(attachment.get_payload(decode=True,))
                f.close()
            except Exception as detail:
                #print detail
                pass